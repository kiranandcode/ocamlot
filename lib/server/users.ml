[@@@warning "-33"]
open Containers
open Common

module StringSet = Set.Make(String)

let log = Logging.add_logger "web.user"

(* let with_user req then_ =
 *   let load_user req username = Dream.sql req (Database.LocalUser.lookup_user ~username) in
 *   with_param "username" load_user req ~then_ ~else_:(not_found ~msg:"User not found") *)

let handle_actor_get_html _config req =
  let username = Dream.param req "username" in
  let+ user =
    Dream.sql req
      (Database.LocalUser.lookup_user ~username)
    |> map_err (fun err -> `DatabaseError err)
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  (* let> _current_user = Common.with_current_user req in *)
  let+ no_following, no_followers, no_posts =
    Dream.sql req (fun db ->
      let+ user = Database.Actor.of_local (Database.LocalUser.self user) db in
      let+ following = Database.Follow.count_following user db in
      let+ followers = Database.Follow.count_followers user db in
      let+ posts = Database.Post.count_posts_by_author user db in
      Lwt.return_ok (following, followers,posts)
    ) |> map_err (fun err -> `DatabaseError err) in

  let timestamp = Dream.query req "start"
                  |> Fun.flip Option.bind (fun v -> Ptime.of_rfc3339 v |> Result.to_opt)
                  |> Option.map (fun (t, _, _) -> t)
                  |> Option.value ~default:(Ptime_clock.now ())
                  |> Ptime.to_float_s
                  |> CalendarLib.Calendar.from_unixfloat in
  let offset = Dream.query req "offset"
               |> Fun.flip Option.bind Int.of_string
               |> Option.value ~default:0 in

  let+ _state =
    Dream.sql req begin fun db ->
      let+ user = Database.Actor.of_local (Database.LocalUser.self user) db in
      match Dream.query req "state" with
      | Some "followers" ->
        let+ follows = 
          Database.Follow.collect_followers
            ~offset:(timestamp, 10, offset * 10) user db in
        let+ follows =
          Lwt_list.map_p (fun follow ->
            let target = Database.Follow.target follow in
            let+ target = Database.Link.resolve target db in
            Lwt.return_ok (follow, target)) follows
          |> lift_pure in
        let+ follows = Lwt.return @@ Result.flatten_l follows in
        Lwt.return_ok (`Followers (timestamp, offset, follows))
      | Some "following" ->
        let+ follows = 
          Database.Follow.collect_following
            ~offset:(timestamp, 10, offset * 10) user db in
        let+ follows =
          Lwt_list.map_s (fun follow ->
            let target = Database.Follow.target follow in
            let+ target = Database.Link.resolve target db in
            Lwt.return_ok (follow, target)) follows
          |> lift_pure in
        let+ follows = Lwt.return @@ Result.flatten_l follows in
        Lwt.return_ok (`Following (timestamp, offset, follows))
      | Some "post" | _ ->
        let+ posts =
          Database.Post.collect_posts_by_author
            ~offset:(timestamp, 10, offset * 10) user db in
        let+ posts =
          Lwt_list.map_s (fun post ->
            let author = Database.Post.author post in
            let+ author = Database.Link.resolve author db in
            Lwt_result.return (author, post)
          ) posts
          |> lift_pure in
        let+ posts = Lwt.return @@ Result.flatten_l posts in
        Lwt.return_ok (`Posts (timestamp, offset, posts))
    end
    |> map_err (fun err -> `DatabaseError err)  in
  let+ headers = Navigation.build_navigation_bar req in
  tyxml @@
  Html.build_page ~headers ~title:(username ^ "'s Profile")  [
    Html.Profile.profile object
      method name = Database.LocalUser.display_name user
      method details = []
      method image = "/static/images/unknown.png"
      method stats = object
        method followers = no_followers
        method following = no_following
        method posts = no_posts
      end
    end
  ]
(* tyxml (invalid_arg "TODO") (\* (Html.Profile.build config current_user ~state ~posts ~following ~followers user req) *\) *)

let handle_actor_get_json config req =
  let username = Dream.param req "username" in
  let+ user =
    Dream.sql req
      (Database.LocalUser.lookup_user ~username)
    |> map_err (fun err -> `DatabaseError err)
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  activity_json
    (user
     |> Database.Interface.LocalUser.convert_to config
     |> Activitypub.Encode.person)

let handle_actor_get config req =
  let content_type = Dream.header req "Accept"
                     |> Option.value ~default:(Activitypub.Constants.ContentType.html) in
  match Activitypub.Constants.ContentType.of_string content_type with
  | None ->
    Error_handling.handle_error_html config
      (fun _ -> return @@ Error (`UnsupportedContentType content_type)) req
  | Some `HTML ->
    Error_handling.handle_error_html config
      (handle_actor_get_html config) req
  | Some `JSON ->
    Error_handling.handle_error_json config
      (handle_actor_get_json config) req

(* let handle_inbox_get req =
 *   Dream.log "GET to %s/inbox" (Dream.param req "username");
 *   Dream.respond ~status:`OK ""
 * 
 * let enforce_is_true vl kont =
 *   if vl
 *   then kont ()
 *   else Dream.respond ~status:`Not_Acceptable {|{"error": "invalid request"}|}
 * 
 * let extract_local_username config url =
 *   let (let+) x f = Option.bind x f in
 *   let user_re = Configuration.Regex.local_user_id_format config in
 *   let+ matches = Re.exec_opt (Re.compile user_re) url in
 *   Re.Group.get_opt matches 1 *)


(* let handle_inbox_post config req =
 *   Dream.log "POST to %s/inbox" (Dream.param req "username");
 *   let> request_is_verified =
 *     Http_sig.verify_request ~resolve_public_key:Resolver.lookup_request req
 *     |> or_errorP ~req in
 *   let> () = request_is_verified |> holds_or ~else_:not_acceptable in
 *   let+ body = Dream.body req in
 *   Dream.log "DATA: %s" body;
 *   print_endline @@  Yojson.Safe.pretty_to_string (Yojson.Safe.from_string body);
 *   let follow =
 *     Decoders_yojson.Safe.Decode.decode_string
 *       Activitypub.Decode.(obj) body in
 *   match follow with
 *   | Error e ->
 *     Dream.error (fun log ->
 *       log ~request:req "error while decoding request: %a"
 *         Decoders_yojson.Safe.Decode.pp_error e);
 *     Dream.respond ~status:`Not_Acceptable ""
 *   | Ok obj ->
 *     Dream.log "recieved an obj:\n%a"
 *       Activitypub.Types.pp_obj obj;
 *     match obj with
 *     | `Accept { id=_; actor=_; published=_;
 *                 obj=`Follow { id; actor; cc=_; to_=_; object_; state=_; raw=_ }; raw=_ } ->
 * 
 *       let> follow = Dream.sql req (Database.Follow.lookup_follow_by_url id)
 *                     |> or_errorP ~req ~err:not_acceptable in
 *       let> follow = follow |> or_not_found in
 *       let> remote = Dream.sql req (Database.RemoteUser.lookup_remote_user_by_url object_)
 *                    |> or_errorP ~req ~err:not_acceptable in
 *       let> remote = remote |> or_not_found in
 *       let> local = extract_local_username config actor |> or_not_acceptable in
 *       let> local = Dream.sql req (Database.LocalUser.lookup_user ~username:local)
 *                   |> or_errorP ~req in
 *       let> local = local |> or_not_found in
 *       Worker.(send req (RecordAcceptLocalFollow {follow; author=local; target=remote;}));
 *       Dream.respond ~status:`OK "Ok"
 *     | `Follow ({ id; actor; cc=_; to_=_; object_; state=(Some `Pending | None); raw }:
 *                  Activitypub.Types.follow) ->
 *       let user_re = Configuration.Regex.local_user_id_format config in
 *       let> user = Re.exec_opt (Re.compile user_re) object_
 *                   |> or_not_acceptable ~msg:"Malformed user id" in
 *       let username = Re.Group.get user 1 in
 *       let> local = Dream.sql req (Database.LocalUser.lookup_user ~username) |> or_errorP ~req in
 *       let> target = local |> or_not_found ~msg:"User not found" in
 *       Worker.(send req (RemoteFollow {id; remote=actor; target; data=raw}));
 *       Dream.respond ~status:`OK "ok"
 *     | `Create {
 *       id=_; actor=_; published=_; to_=_; cc=_;
 *       direct_message; obj=`Note {
 *         id; actor;
 *         to_; cc; in_reply_to=_;
 *         content; sensitive; source; summary;
 *         published; tags; raw
 *       }; raw=_
 *     } ->
 *       Worker.(send req (CreateNote {
 *         id; author=actor; to_; cc; sensitive; direct_message;
 *         content; source; summary; published; tags; data=raw
 *       }));
 *       Dream.respond ~status:`OK "ok"
 *     | `Follow _ ->
 *       Dream.respond ~status:`Not_Acceptable "??"
 *     | `Create _ ->
 *       Dream.log "received a create object!";
 *       Dream.respond ~status:`OK "ok"
 *     | `Announce _ ->
 *       Dream.log "received an announce object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Block _ ->
 *       Dream.log "received a block object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Note _ ->
 *       Dream.log "received a note object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Person _ ->
 *       Dream.log "received a person object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Undo _ ->
 *       Dream.log "received an undo object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Delete _ ->
 *       Dream.log "received a delete object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Accept _ ->
 *        Dream.log "received an accept object!";
 *       Dream.respond ~status:`Not_Implemented "lol"
 *     | `Like _ ->
 *       Dream.log "received a like object!";
 *       Dream.respond ~status:`Not_Implemented "lol" *)


let handle_outbox_get req =
  Dream.log "GET %s/outbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

(* let handle_outbox_post req =
 *   Dream.log "POST %s/outbox" (Dream.param req "username");
 *   let+ body = Dream.body req in
 *   Dream.log "DATA: %s" body;
 *   Dream.respond ~status:`OK "" *)

let handle_followers_get config req =
  let username = Dream.param req "username" in
  let+ user = Dream.sql req (Database.LocalUser.lookup_user ~username)
              |> map_err (fun err -> `DatabaseError err) in
  let+ user = return @@ lift_opt ~else_:(fun _ -> `UserNotFound username)
                          user in
  let offset =
    let open Option in
    let* page = Dream.query req "page"
    and* start_time = Dream.query req "start" in
    let* page = Int.of_string page 
    and* start_time, _, _ = Ptime.of_rfc3339 start_time
                            |> Result.to_opt in
    let start_time = Ptime.to_float_s start_time
                   |> CalendarLib.Calendar.from_unixfloat in
    return (page, start_time) in
  let is_page = Option.is_some offset in
  let offset, start_time = Option.value ~default:(0, CalendarLib.Calendar.now ())
                       offset in
  let+ followers_collection_page =
    Dream.sql req
      (Resolver.build_followers_collection_page
         config start_time offset user)
    |> map_err (fun err -> `DatabaseError err) in
  let data =
    if is_page
    then Activitypub.Encode.ordered_collection_page (Decoders_yojson.Safe.Encode.string)
           followers_collection_page
    else Activitypub.Encode.ordered_collection (Decoders_yojson.Safe.Encode.string)
           ({
             id = Some (
               Configuration.Url.user_followers config (Database.LocalUser.username user)
               |> Uri.to_string
             );
             total_items=followers_collection_page.total_items
                         |> Option.get_exn_or "invalid assumption";
             contents=`First followers_collection_page; 
           } : string Activitypub.Types.ordered_collection) in
  activity_json data

let handle_following_get config req =
  let username = Dream.param req "username" in
  let+ user = Dream.sql req (Database.LocalUser.lookup_user ~username)
              |> map_err (fun err -> `DatabaseError err) in
  let+ user = return @@ lift_opt ~else_:(fun _ -> `UserNotFound username)
                          user in
  let offset =
    let open Option in
    let* page = Dream.query req "page"
    and* start_time = Dream.query req "start" in
    let* page = Int.of_string page 
    and* start_time, _, _ = Ptime.of_rfc3339 start_time
                            |> Result.to_opt in
    let start_time = Ptime.to_float_s start_time
                   |> CalendarLib.Calendar.from_unixfloat in
    return (page, start_time) in
  let is_page = Option.is_some offset in
  let offset, start_time = Option.value ~default:(0, CalendarLib.Calendar.now ())
                       offset in
  let+ following_collection_page =
    Dream.sql req
      (Resolver.build_following_collection_page
         config start_time offset user)
    |> map_err (fun err -> `DatabaseError err) in
  let data =
    if is_page
    then Activitypub.Encode.ordered_collection_page (Decoders_yojson.Safe.Encode.string)
           following_collection_page
    else Activitypub.Encode.ordered_collection (Decoders_yojson.Safe.Encode.string)
           ({
             id = Some (
               Configuration.Url.user_following config (Database.LocalUser.username user)
               |> Uri.to_string
             );
             total_items=following_collection_page.total_items
                         |> Option.get_exn_or "invalid assumption";
             contents=`First following_collection_page; 
           } : string Activitypub.Types.ordered_collection) in
  activity_json data

let parse_user_types = function
  | "local" -> Some `Local
  | "remote" -> Some `Remote
  | _ -> None

let encode_user_types = function
  | `Local -> "local"
  | `Remote -> "remote"

let show_user_types = function
  | `Local -> "Local Users"
  | `Remote -> "Remote Users"


let classify_query s =
  let s = String.trim s in
  let has_spaces = String.contains s ' ' in
  let contains_at = (String.contains s '@') in
  if has_spaces || not contains_at
  then `SearchLike (String.split_on_char ' ' s)
  else begin
    let domain = String.split_on_char '@' s in
    match domain with
    | [username; domain] -> `Resolve (username, domain)
    | username :: _ -> `SearchLike [username]
    | _ -> `SearchLike [s]
  end

let render_users_page ?search_query req user_type users =
  let+ headers = Navigation.build_navigation_bar req in
  tyxml (Html.build_page ~headers ~title:(show_user_types user_type) Tyxml.Html.[
    Html.Components.page_title (show_user_types user_type);
    Html.Components.subnavigation_menu [
      "Local", "/users?type=local";
      "Remote", "/users?type=remote";
    ];
    Html.Components.search_box
      ~fields:["type", encode_user_types user_type]
      ?value:search_query ();
    div ~a:[a_class ["users-list"]]
      users
  ])

let handle_local_users_get _config req =
  let+ current_user_link = current_user_link req in
  let offset_start =
    Dream.query req "offset-start"
    |> Option.flat_map Int.of_string
    |> Option.value ~default:0 in
  let limit = 10 in
  let search_query = Dream.query req "search" in
  let+ users =
    match search_query with
    | Some query when not (String.is_empty query) ->
      let query = "%" ^ (String.replace ~sub:" " ~by:"%" query) ^ "%" in
      Dream.sql req (fun db ->
        Database.LocalUser.find_local_users
          ~offset:(limit, offset_start * limit)
          query db)
      |> map_err (fun err -> `DatabaseError err)
    | _ ->
      Dream.sql req (fun db ->
        Database.LocalUser.collect_local_users
          ~offset:(limit, offset_start * limit) db)
      |> map_err (fun err -> `DatabaseError err) in
  let+ users_w_stats =
    Lwt_list.map_p (fun user ->
      Dream.sql req @@ fun db ->
      let+ user_link = Database.Actor.of_local (Database.LocalUser.self user) db in
      let+ no_followers = Database.Follow.count_followers user_link db in
      let+ no_posts = Database.Post.count_posts_by_author user_link db in
      let+ is_following =
        match current_user_link with
        | None -> return_ok None
        | Some current_user_link ->
          Database.Follow.is_following ~author:current_user_link ~target:user_link db
          |> Lwt_result.map Option.some in
      return_ok (user, no_followers, no_posts, is_following)
    ) users
    |> lift_pure
    >>= (fun r -> return (Result.flatten_l r))
    |> map_err (fun e -> `DatabaseError e) in
  let+ current_user = current_user req in
  let can_follow =
    match current_user with
    | None -> fun _ -> false
    | Some current_user -> fun other_user ->
      not @@ String.equal
               (Database.LocalUser.username current_user)
               (Database.LocalUser.username other_user) in
  let users =
    List.map (fun (user, no_followers, no_posts, is_following) ->
      Html.Users.user ~can_follow:(can_follow user)
        (object
          method about = []
          method display_name = Database.LocalUser.display_name user
          method username = Database.LocalUser.username user
          method follow_link = ("/users/" ^ (Database.LocalUser.username user) ^ "/follow")
          method profile_page = ("/users/" ^ (Database.LocalUser.username user))
          method following = is_following
          method profile = object
            method image = "/static/images/unknown.png"
            method name = ""
          end
          method stats = object
            method followers = no_followers
            method posts = no_posts
          end
        end)
    ) users_w_stats in
  render_users_page ?search_query req `Local users

let handle_remote_users_get config req =
  let+ current_user_link = current_user_link req in
  let offset_start =
    Dream.query req "offset-start"
    |> Option.flat_map Int.of_string
    |> Option.value ~default:0 in
  let limit = 10 in
  let search_query = Dream.query req "search" in
  let+ users =
    match search_query, current_user_link with
    | None, _ | Some _, None ->
      Dream.sql req (fun db ->
        Database.RemoteUser.collect_remote_users
          ~offset:(limit, offset_start * limit) db)
      |> map_err (fun err -> `DatabaseError err)
    | Some query, Some _ ->
      match classify_query query with
      | `Resolve (user, domain) ->
        log.debug (fun f -> f "received explicit search - sending task to worker");
        Configuration.Params.send_task config Worker.(SearchRemoteUser {
          username=user; domain=Some domain
        });
        let query = "%" ^ user ^ "%" in
        Dream.sql req (fun db ->
          Database.RemoteUser.find_remote_users
            ~offset:(limit, offset_start * limit) query db
        )
        |> map_err (fun err -> `DatabaseError err)
      | `SearchLike query ->
        log.debug (fun f -> f "received implicit search");
        begin match query with
        | [username] -> 
          log.debug (fun f -> f "implicit search over single parameter - sending task to worker");
          Configuration.Params.send_task config Worker.(SearchRemoteUser {
            username; domain=None
          });
        | _ -> ()
        end;
        let query = "%" ^ (String.concat "%" query) ^ "%" in
        Dream.sql req (fun db ->
          Database.RemoteUser.find_remote_users
            ~offset:(limit, offset_start * limit) query db
        )
        |> map_err (fun err -> `DatabaseError err) in
  let+ users_w_stats =
    Lwt_list.map_p (fun (user, url) ->
      Dream.sql req @@ fun db ->
      let+ user_link = Database.Actor.of_remote (Database.RemoteUser.self user) db in
      let+ no_followers = Database.Follow.count_followers user_link db in
      let+ no_posts = Database.Post.count_posts_by_author user_link db in
      let+ is_following =
        match current_user_link with
        | None -> return_ok None
        | Some current_user_link ->
          let+ follow =
            Database.Follow.find_follow_between
              ~author:current_user_link ~target:user_link db in
          return_ok @@ match follow with
          | Some follow when Database.Follow.pending follow -> None
          | Some _ -> Some true
          | None -> Some false in
      return_ok (user, url, no_followers, no_posts, is_following)
    ) users
    |> lift_pure
    >>= (fun r -> return (Result.flatten_l r))
    |> map_err (fun e -> `DatabaseError e) in
  let users =
    List.map (fun (user, url, no_followers, no_posts, is_following) ->
      Html.Users.user ~can_follow:true
        (let fqn = (Database.RemoteUser.username user) ^ "@" ^ url in
         object
           method about = []
           method display_name = Database.RemoteUser.display_name user
           method username = Database.RemoteUser.username user ^ "@" ^ url
           method follow_link = ("/users/" ^ fqn ^ "/follow")
           method profile_page = ("/users/" ^ fqn)
           method following = is_following
           method profile = object
             method image = "/static/images/unknown.png"
             method name = ""
           end
           method stats = object
             method followers = no_followers
             method posts = no_posts
           end
         end)
    ) users_w_stats in
  render_users_page ?search_query req `Remote users

let handle_users_get _config req =
  let user_list_ty =
    Dream.query req "type"
    |> Option.flat_map parse_user_types
    |> Option.value ~default:`Local in
  match user_list_ty with
  | `Local ->
    handle_local_users_get _config req
  | `Remote ->
    handle_remote_users_get _config req

let handle_follow_local_user _config current_user username req =
  let+ current_user = Dream.sql req Database.(Actor.of_local (LocalUser.self current_user))
                      |> map_err (fun err -> `DatabaseError err) in
  let+ target_user =
    Dream.sql req
      (Database.LocalUser.lookup_user ~username)
    |> map_err (fun err -> `DatabaseError err)
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  let+ target_user =
    Dream.sql req @@ Database.Actor.of_local (Database.LocalUser.self target_user)
    |> map_err (fun err -> `DatabaseError err) in
  let+ is_following =
    Dream.sql req @@ fun db ->
    Database.Follow.is_following ~author:current_user ~target:target_user db
    |> map_err (fun e -> `DatabaseError e) in
  match is_following with
  | false ->
    let id = Uuidm.v `V4 in
    let+ _follow =
      Dream.sql req @@
      Database.Follow.create_follow
        ~public_id:(Uuidm.to_string id)
        ~url:("/api/follows/" ^ (Uuidm.to_string id))
        ~author:current_user
        ~target:target_user
        ~pending:false
        ~created:(CalendarLib.Calendar.now ())
      |> map_err (fun e -> `DatabaseError e) in
    redirect req "/users"
  | true ->
    let+ follow =
      Dream.sql req @@
      Database.Follow.find_follow_between_exn
        ~author:current_user
        ~target:target_user
      |> map_err (fun e -> `DatabaseError e) in
    let+ _ =
      Dream.sql req @@
      Database.Follow.delete_follow (Database.Follow.self follow)
      |> map_err (fun e -> `DatabaseError e) in
    redirect req "/users"

let handle_follow_remote_user config current_user username domain req =
  Configuration.Params.send_task config Worker.(
    FollowRemoteUser {
      user=current_user;
      username;
      domain
    }
  );
  redirect req "/users"

let handle_users_follow_post _config req =
  let username = Dream.param req "username" in
  let+ current_user =
    let+ current_user = current_user req in
    lift_opt ~else_:(fun _ -> `InvalidPermissions ("Attempt to follow user while logged out"))
      current_user
    |> return in
  match String.contains username '@', lazy (String.split_on_char '@' username) with
  | true, lazy (username :: domain)  ->
    let domain = String.concat "@" domain in
    handle_follow_remote_user _config current_user username domain req
  | _ ->
    handle_follow_local_user _config current_user username req

let handle_inbox_post config req =
  log.debug (fun f -> f "validating request");
  let+ valid_request =
    Http_sig.verify_request
      ~resolve_public_key:Resolver.resolve_public_key req
    |> map_err (fun err -> `ResolverError err) in
  log.debug (fun f -> f "request validation completed with result %b" valid_request);
  let+ () =
    if valid_request
    then return_ok ()
    else return (Error (`InvalidSignature)) in
  let username = Dream.param req "username" in
  let+ _user = 
    Dream.sql req (Database.LocalUser.lookup_user ~username)
    |> map_err (fun err -> `DatabaseError err)
    >>= (fun v -> return (lift_opt ~else_:(fun () -> `UserNotFound username) v)) in
  let+ body_text = lift_pure (Dream.body req) in
  let+ data =
    (Activitypub.Decode.(decode_string obj) body_text)
    |> (function
      | Ok _ as data -> data
      | Error err ->
        if Configuration.Params.debug config then
          IO.with_out ~flags:[Open_append; Open_creat] "ocamlot-failed-events.json" (Fun.flip IO.write_line (body_text ^ "\n"));
        Error err)
    |> return
    |> map_err (fun err -> `InvalidActivitypubObject err) in

(* map_err (fun err -> `ActivitypubFormat err) *)
  log.debug (fun f ->
    f "received activitypub object %a"
      Activitypub.Types.pp_obj data
  );
  log.debug (fun f ->
    f "headers were %s"
      ([%show: (string * string) list] (Dream.all_headers req))
  );

  let+ _ =
    match[@warning "-27"] data with
    | `Accept { obj=`Follow { id=follow_id; _ }; id=accept_activity_id; raw=accept_obj; actor; _ } ->
      Configuration.Params.send_task config Worker.(
        HandleAcceptFollow { follow_id }
      );
      return_ok ()
    | `Follow { id; actor; cc; to_; object_; state; raw } ->
      let user_re = Configuration.Regex.local_user_id_format config in
      let+ re_matches = Re.exec_opt (Re.compile user_re) object_
                        |> lift_opt ~else_:(fun () -> `InvalidData "follow target was not a valid user")
                        |> return in
      let username = Re.Group.get re_matches 1 in
      let+ target = Dream.sql req (Database.LocalUser.lookup_user ~username)
                    |> map_err (fun err -> `DatabaseError err) in
      let+ target = lift_opt ~else_:(fun _ -> `UserNotFound object_) target
                    |> return in
      Configuration.Params.send_task config Worker.(
        HandleRemoteFollow { id; actor; target; raw }
      );
      return_ok ()
    | `Undo { obj=`Follow { id=follow_id; _ }; _ } ->
      Configuration.Params.send_task config Worker.(
        HandleUndoFollow { follow_id }
      );
      return_ok ()
    | `Create { obj=`Note note; actor; published; direct_message; _ } ->
      let+ remote_user =
        Dream.sql req
          (Database.RemoteUser.lookup_remote_user_by_url actor)
        |> map_err (fun err -> `DatabaseError err)
        (* if the remote user isn't registered in our database, then ignore it *)
        >> Result.flat_map (lift_opt ~else_:(fun _ -> `UnknownRemoteUser actor)) in
      Configuration.Params.send_task config Worker.(
        CreateRemoteNote { author=remote_user; direct_message; note }
      );
      return_ok ()
    | `Accept _
    | `Announce _
    | `Block _
    | `Note _
    | `Person _
    | `Undo _
    | `Delete _
    | `Create _
    | `Like _ -> return_ok ()
  in

  json (`Assoc [
    "ok", `List []
  ])


let route config = 
  Dream.scope "/users" [] [
    Dream.get "" @@ Error_handling.handle_error_html config (handle_users_get config);
    Dream.get "/:username" @@ (handle_actor_get config);
    Dream.post "/:username/follow" @@ Error_handling.handle_error_html config (handle_users_follow_post config);
    (* Dream.get "/:username/inbox" handle_inbox_get; *)
    Dream.post ":username/inbox" @@ Error_handling.handle_error_json config (handle_inbox_post config);
    Dream.get "/:username/outbox" handle_outbox_get;
    (* Dream.post "/:username/outbox" handle_outbox_post; *)

    Dream.get "/:username/followers" @@ Error_handling.handle_error_json config (handle_followers_get config);
    Dream.get "/:username/following" @@ Error_handling.handle_error_json config (handle_following_get config);
  ]
