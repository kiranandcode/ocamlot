open Containers
open Common
module StringSet = Set.Make(String)

let with_user req then_ =
  let load_user req username = Dream.sql req (Database.LocalUser.lookup_user ~username) in
  with_param "username" load_user req ~then_ ~else_:(not_found ~msg:"User not found")

let handle_actor_get config req =
  let> user = with_user req in
  let content_type = Dream.header req "Accept"
                     |> Option.value ~default:(Activitypub.Constants.ContentType.html) in
  match Activitypub.Constants.ContentType.of_string content_type with
  | None -> Dream.respond ~status:`Not_Acceptable "{}"
  | Some `HTML ->
    let> current_user = Common.with_current_user req in
    let> following, followers, posts =
      Dream.sql req (fun db ->
        let+! user = Database.Actor.of_local (Database.LocalUser.self user) db in
        let+! following = Database.Follow.count_following user db in
        let+! followers = Database.Follow.count_followers user db in
        let+! posts = Database.Post.count_posts_by_author user db in
        Lwt.return_ok (following, followers,posts)
      ) |> or_errorP ~err:internal_error ~req in

    let timestamp = Dream.query req "start"
                    |> Fun.flip Option.bind (fun v -> Ptime.of_rfc3339 v |> Result.to_opt)
                    |> Option.map (fun (t, _, _) -> t)
                    |> Option.value ~default:(Ptime_clock.now ())
                    |> Ptime.to_float_s
                    |> CalendarLib.Calendar.from_unixfloat in
    let offset = Dream.query req "offset"
                 |> Fun.flip Option.bind Int.of_string
                 |> Option.value ~default:0 in

    let> state =
      Dream.sql req begin fun db ->
        let+! user = Database.Actor.of_local (Database.LocalUser.self user) db in
        match Dream.query req "state" with
        | Some "followers" ->
          let+! follows = 
            Database.Follow.collect_followers
              ~offset:(timestamp, 10, offset * 10) user db in
          let+ follows =
            Lwt_list.map_s (fun follow ->
              let target = Database.Follow.target follow in
              let+! target = Database.Link.resolve target db in
              Lwt.return_ok (follow, target)) follows in
          let+! follows = Lwt.return @@ Result.flatten_l follows in
          Lwt.return_ok (`Followers (timestamp, offset, follows))
        | Some "following" ->
          let+! follows = 
            Database.Follow.collect_following
              ~offset:(timestamp, 10, offset * 10) user db in
          let+ follows =
            Lwt_list.map_s (fun follow ->
              let target = Database.Follow.target follow in
              let+! target = Database.Link.resolve target db in
              Lwt.return_ok (follow, target)) follows in
          let+! follows = Lwt.return @@ Result.flatten_l follows in
          Lwt.return_ok (`Following (timestamp, offset, follows))
        | Some "post" | _ ->

          let+! posts =
            Database.Post.collect_posts_by_author
              ~offset:(timestamp, 10, offset * 10) user db in
          let+ posts = Lwt_list.map_s (fun post ->
            let author = Database.Post.author post in
            let+! author = Database.Link.resolve author db in
            Lwt_result.return (author, post)
          ) posts in
          let+! posts = Lwt.return @@ Result.flatten_l posts in
          Lwt.return_ok (`Posts (timestamp, offset, posts))
      end
      |> or_errorP ~req ~err:internal_error in

    (* let> follows =
     *   Dream.sql req (fun db ->
     *     let+! current_user = Database.Actor.of_local (Database.LocalUser.self user) db in
     *     Database.Follow.collect_follows_for_actor ~offset:(timestamp, 10, offset * 10) current_user db)
     *   |> or_errorP ~err:internal_error ~req in *)


    Dream.html (Html.Profile.build config current_user ~state ~posts ~following ~followers user req)
  | Some `JSON ->
    activity_json
      (user
       |> Database.Interface.LocalUser.convert_to config
       |> Activitypub.Encode.person)

let handle_inbox_get req =
  Dream.log "GET to %s/inbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

let enforce_is_true vl kont =
  if vl
  then kont ()
  else Dream.respond ~status:`Not_Acceptable {|{"error": "invalid request"}|}

let extract_local_username config url =
  let (let+) x f = Option.bind x f in
  let user_re = Configuration.Regex.local_user_id_format config in
  let+ matches = Re.exec_opt (Re.compile user_re) url in
  Re.Group.get_opt matches 1
    

let handle_inbox_post config req =
  Dream.log "POST to %s/inbox" (Dream.param req "username");
  let> request_is_verified =
    Http_sig.verify_request ~resolve_public_key:Resolver.lookup_request req
    |> or_errorP ~req in
  let> () = request_is_verified |> holds_or ~else_:not_acceptable in
  let+ body = Dream.body req in
  Dream.log "DATA: %s" body;
  let follow =
    Decoders_yojson.Safe.Decode.decode_string
      Activitypub.Decode.(obj) body in
  match follow with
  | Error e ->
    Dream.error (fun log ->
      log ~request:req "error while decoding request: %a"
        Decoders_yojson.Safe.Decode.pp_error e);
    Dream.respond ~status:`Not_Acceptable ""
  | Ok obj ->
    Dream.log "recieved an obj:\n%a"
      Activitypub.Types.pp_obj obj;
    match obj with
    | `Accept { id=_; actor=_; published=_;
                obj=`Follow { id; actor; cc=_; to_=_; object_; state=_; raw=_ }; raw=_ } ->

      let> follow = Dream.sql req (Database.Follow.lookup_follow_by_url id)
                    |> or_errorP ~req ~err:not_acceptable in
      let> follow = follow |> or_not_found in
      let> remote = Dream.sql req (Database.RemoteUser.lookup_remote_user_by_url object_)
                   |> or_errorP ~req ~err:not_acceptable in
      let> remote = remote |> or_not_found in
      let> local = extract_local_username config actor |> or_not_acceptable in
      let> local = Dream.sql req (Database.LocalUser.lookup_user ~username:local)
                  |> or_errorP ~req in
      let> local = local |> or_not_found in
      Worker.(send req (RecordAcceptLocalFollow {follow; author=local; target=remote;}));
      Dream.respond ~status:`OK "Ok"
    | `Follow ({ id; actor; cc=_; to_=_; object_; state=(Some `Pending | None); raw }:
                 Activitypub.Types.follow) ->
      let user_re = Configuration.Regex.local_user_id_format config in
      let> user = Re.exec_opt (Re.compile user_re) object_
                  |> or_not_acceptable ~msg:"Malformed user id" in
      let username = Re.Group.get user 1 in
      let> local = Dream.sql req (Database.LocalUser.lookup_user ~username) |> or_errorP ~req in
      let> target = local |> or_not_found ~msg:"User not found" in
      Worker.(send req (RemoteFollow {id; remote=actor; target; data=raw}));
      Dream.respond ~status:`OK "ok"
    | `Create {
      id=_; actor=_; published=_; to_=_; cc=_;
      direct_message; obj=`Note {
        id; actor;
        to_; cc; in_reply_to=_;
        content; sensitive; source; summary;
        published; tags; raw
      }; raw=_
    } ->
      Worker.(send req (CreateNote {
        id; author=actor; to_; cc; sensitive; direct_message;
        content; source; summary; published; tags; data=raw
      }));
      

      Dream.log "received a create object!";
      Dream.respond ~status:`Not_Implemented "lol"

    | `Follow _ ->
      Dream.respond ~status:`Not_Acceptable "??"
    | `Create _ ->
      Dream.log "received a create object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Announce _ ->
      Dream.log "received an announce object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Block _ ->
      Dream.log "received a block object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Note _ ->
      Dream.log "received a note object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Person _ ->
      Dream.log "received a person object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Undo _ ->
      Dream.log "received an undo object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Delete _ ->
      Dream.log "received a delete object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Accept _ ->
       Dream.log "received an accept object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Like _ ->
      Dream.log "received a like object!";
      Dream.respond ~status:`Not_Implemented "lol"


let handle_outbox_get req =
  Dream.log "GET %s/outbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

let handle_outbox_post req =
  Dream.log "POST %s/outbox" (Dream.param req "username");
  let+ body = Dream.body req in
  Dream.log "DATA: %s" body;
  Dream.respond ~status:`OK ""

let route config = 
    Dream.scope "/users" [] [
      Dream.get "/:username" (handle_actor_get config);
      Dream.get "/:username/inbox" handle_inbox_get;
      Dream.post ":username/inbox" (handle_inbox_post config);
      Dream.get "/:username/outbox" handle_outbox_get;
      Dream.post "/:username/outbox" handle_outbox_post;
    ]
