[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "back.resolver"

let fresh_id () = Uuidm.v `V4 |> Uuidm.to_string
let sanitize err =
  Lwt_result.map_error (fun err -> Caqti_error.show err) err
let lift_database_error res =
  map_err (fun err -> `DatabaseError (Caqti_error.show err)) res

let map_list f ls =
  Lwt_list.map_s f ls
  |> Lwt.map (List.filter_map (function
      | Ok v -> Some v
      | Error err ->
        let _, msg, details = Error_handling.extract_error_details err in
        log.debug (fun f -> f "extracting results failed with %s: %s" msg details);
        None
    ))
  |> lift_pure

let decode_body ?ty ~into:decoder body =
  let* body = Cohttp_lwt.Body.to_string body >> Result.return in
  begin match ty with
    | Some ty -> Configuration.dump_string ~ty body
    | None -> ()
  end;
  body
  |> Activitypub.Decode.(decode_string decoder)
  |> Lwt.return

let resolve_public_key url =
  (* NOTE: Not obvious, but you need to specify accept headers, else
     pleroma will return html *)
  let* (_resp, body) = Requests.activity_req (Uri.of_string url) in
  let* actor = decode_body ~into:Activitypub.Decode.person body in
  let pub_key =
    actor.public_key.pem
    |> Cstruct.of_string
    |> X509.Public_key.decode_pem
    |> Result.map_err (fun (`Msg err) -> err) in
  Lwt.return pub_key

let resolve_remote_user_with_webfinger ~local_lookup ~webfinger_uri db
  : (Database.RemoteUser.t, string) Lwt_result.t =
  let* domain = Uri.host webfinger_uri |> Result.of_opt |> Lwt.return in
  let* result = local_lookup db in
  match result with
  | Some v -> Lwt.return_ok v
  | None ->
    (* remote user not found *)
    (* webfinger to find user url *)
    let* remote_user_url =
      let* (_, body) = Requests.json_rd_req webfinger_uri in
      let* query_res =
        decode_body ~ty:"webfinger" body
          ~into:Activitypub.Decode.Webfinger.query_result in
      get_opt (Activitypub.Types.Webfinger.self_link query_res)
        ~else_:(fun () -> "could not retrieve self link.") in
    (* retrieve json *)
    let* (_, body) = Requests.activity_req remote_user_url in
    let* person_res =
      decode_body ~ty:"remote-user" body
        ~into:Activitypub.Decode.person in
    let* remote_instance = Database.RemoteInstance.create_instance ~url:domain db |> sanitize in
    let* () =
      Database.RemoteInstance.unset_instance_last_unreachable
        ~id:remote_instance.Database.RemoteInstance.id db
      |> sanitize in
    let* username =
      get_opt person_res.preferred_username
        ~else_:(fun () -> "could not retrieve username") in
    let* url =
      get_opt person_res.url
        ~else_:(fun () -> "could not retrieve user url.") in
    Database.RemoteUser.create_remote_user
      ?display_name:person_res.name
      ~inbox:person_res.inbox
      ~outbox:person_res.outbox
      ?followers:person_res.followers
      ?following:person_res.following
      ?summary:person_res.summary
      ?profile_picture:person_res.icon
      ~public_key_pem:person_res.public_key.pem
      ~username
      ~instance:(remote_instance.Database.RemoteInstance.id)
      ~url:url db |> sanitize

let resolve_remote_user ~username ~domain db =
  resolve_remote_user_with_webfinger
    ~local_lookup:(fun db ->
        Database.RemoteUser.lookup_remote_user_by_address ~username ~url:domain db |> sanitize)
    ~webfinger_uri:(Uri.make
                      ~scheme:"https"
                      ~host:domain
                      ~path:"/.well-known/webfinger"
                      ~query:["resource", [Printf.sprintf "acct:%s@%s" username domain]] ()
                   ) db

let resolve_remote_user_by_url url db =
  let url' = Uri.to_string url in
  resolve_remote_user_with_webfinger
    ~local_lookup:(fun db -> Database.RemoteUser.lookup_remote_user_by_url ~url:url' db |> sanitize)
    ~webfinger_uri:(
      url
      |> Fun.flip Uri.with_path "/.well-known/webfinger"
      |> Fun.flip Uri.with_query' ["resource", url']
      |> Fun.flip Uri.with_scheme (Some "https")
    ) db

(** [resolve_tagged_user config username db] given a [username] in the
    form {<username>@<domain>} classifies the user a a local or remote
    user if they exist. *)
let resolve_tagged_user user db = 
  let user_tag = Configuration.Regex.user_tag in
  let matches = Re.all user_tag (String.trim user) in
  let user_tag = List.head_opt matches in
  match user_tag with
  | None ->
    (* remote user by url *)
    let* remote_user = resolve_remote_user_by_url (Uri.of_string user) db
                       |> map_err (fun err -> `WorkerFailure err) in
    return (Ok (`Remote remote_user))
  | Some group when Option.equal String.equal
        (Some (Lazy.force Configuration.host))
        (Re.Group.get_opt group 2) ->
    (* local user *)
    let username = Re.Group.get group 1 in
    let* resolved_user =
      lift_database_error
        (Database.LocalUser.find_user ~username db) in
    begin match resolved_user with
      | None -> return (Error (`WorkerFailure (Format.sprintf "could not resolve local user %s" user)))
      | Some user -> return_ok (`Local user)
    end
  | Some group ->
    let username = Re.Group.get group 1 in
    let domain = Re.Group.get group 2 in
    let* resolved_user = resolve_remote_user ~username ~domain db
                         |> map_err (fun msg -> `WorkerFailure msg) in
    return_ok (`Remote resolved_user)

let create_accept_follow follow remote local db =
  let local_user =
    Configuration.Url.user (local.Database.LocalUser.username)
    |> Uri.to_string in
  let id = fresh_id () in
  let accept = 
    ({
      id=Configuration.Url.activity_endpoint id |> Uri.to_string;
      actor=local_user;
      published=Some (Ptime_clock.now ());
      obj=({
          id=follow.Database.Follows.url;
          actor=remote.Database.RemoteUser.url;
          cc=[]; to_=[local_user]; state=None; raw=`Null;
          object_=local_user;
        }: Activitypub.Types.follow); raw=`Null;
    }:_ Activitypub.Types.accept) in
  let accept = Activitypub.Encode.(accept follow) accept in
  let* _ = Database.Activity.create ~id ~data:accept db in
  Lwt_result.return accept

let target_list_to_urls to_ db =
  map_list (fun actor ->
      lift_database_error
        (Database.resolve_actor_url ~id:actor db)
    ) to_

let create_note_obj post_id author published scope to_ cc content summary =
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let id =
    post_id
    |> Configuration.Url.activity_endpoint
    |> Uri.to_string in
  let actor = 
    author.Database.LocalUser.username
    |> Configuration.Url.user
    |> Uri.to_string in
  let to_ =
    if is_public
    then (Activitypub.Constants.ActivityStreams.public :: to_)
    else to_ in
  let cc = if is_follower_public
    then
      let followers_url =
        author.Database.LocalUser.username
        |> Configuration.Url.user_followers
        |> Uri.to_string in
      followers_url :: cc
    else cc in
  Activitypub.Types.{
    id;
    actor;
    to_;
    cc;
    in_reply_to=None;
    content=content;
    source=Some content;
    summary;
    published=Some published;
    tags=[];
    sensitive=not is_public && not is_follower_public;
    raw=`Null
  }

let create_create_event_obj post_id author published scope to_ cc post =
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let id =
    post_id
    |> Configuration.Url.activity_endpoint
    |> Uri.to_string in
  let actor = 
    author.Database.LocalUser.username
    |> Configuration.Url.user
    |> Uri.to_string in
  let to_ =
    if is_public
    then (Activitypub.Constants.ActivityStreams.public :: to_)
    else to_ in
  let cc = if is_follower_public
    then
      let followers_url =
        author.Database.LocalUser.username
        |> Configuration.Url.user_followers
        |> Uri.to_string in
      followers_url :: cc
    else cc in
  Activitypub.Types.{
    id;
    actor;
    published=Some published;
    to_;
    cc;
    direct_message=not is_public && not is_follower_public;
    obj=post;
    raw=`Null
  }

let build_follow_request local remote db =
  log.debug (fun f -> f "building follow request");
  let id = try fresh_id () with exn ->
    log.debug (fun f -> f "failed with error %s" (Printexc.to_string exn));
    raise exn in
  log.debug (fun f -> f "created fresh id %s" id);
  let local_actor_url = 
    local.Database.LocalUser.username
    |> Configuration.Url.user
    |> Uri.to_string in
  log.debug (fun f -> f "calculated local actor url %s" local_actor_url);
  let remote_actor_url = remote.Database.RemoteUser.url in
  log.debug (fun f -> f "calculated remote actor url %s" remote_actor_url);
  let follow_request =
    let id =
      id
      |> Configuration.Url.activity_endpoint
      |> Uri.to_string in
    Activitypub.Types.{
      id; actor=local_actor_url;
      cc = []; to_ = [ remote_actor_url ];
      object_=remote_actor_url; state = Some `Pending;
      raw=`Null
    }  in
  let data = Activitypub.Encode.follow follow_request in
  log.debug (fun f -> f "constructed yojson object %a" Yojson.Safe.pp data);
  let* _ =
    log.debug (fun f -> f "resolving remote author");
    let* author = Database.Actor.create_local_user ~local_id:(local.Database.LocalUser.id) db in
    log.debug (fun f -> f "resolving target");
    let* target = Database.Actor.create_remote_user ~remote_id:(remote.Database.RemoteUser.id) db in
    log.debug (fun f -> f "creating follow");
    Database.Follows.create
      ~url:(Configuration.Url.activity_endpoint id
            |> Uri.to_string)
      ~public_id:id
      ~author ~target ~pending:true
      ~created:(Ptime_clock.now ()) db in
  log.debug (fun f -> f "creating follow in activity db");
  let* _ = Database.Activity.create ~id ~data db in
  Lwt_result.return (data |> Yojson.Safe.to_string)

let follow_remote_user
    (local: Database.LocalUser.t)
    ~username ~domain db: (unit,string) Lwt_result.t =
  log.debug (fun f -> f "resolving remote user %s@%s" username domain);
  let* remote = resolve_remote_user ~username ~domain db in
  log.debug (fun f -> f "successfully resolved remote user %s@%s" username domain);
  let* follow_request = build_follow_request local remote db |> sanitize in
  log.debug (fun f -> f "built follow request %s" follow_request);
  let* uri = Lwt_result.lift (Result.of_opt remote.Database.RemoteUser.inbox) in
  let key_id =
    local.Database.LocalUser.username
    |> Configuration.Url.user_key
    |> Uri.to_string in
  let priv_key =
    local.Database.LocalUser.privkey in
  log.debug (fun f -> f "sending signed follow request");
  let* resp, body  = Requests.signed_post (key_id, priv_key) (Uri.of_string uri) follow_request in
  let* body = lift_pure (Cohttp_lwt.Body.to_string body) in
  log.debug (fun f -> f "follow request response was (STATUS: %s) %s"
                (Cohttp.Code.string_of_status resp.status) body);
  match resp.status with
  | `OK -> Lwt_result.return ()
  | _ -> Lwt_result.fail "request failed"

let accept_remote_follow  follow remote local db =
  let* accept_follow = create_accept_follow follow remote local db |> sanitize in
  let* uri = Lwt_result.lift (Result.of_opt remote.Database.RemoteUser.inbox) in
  let key_id =
    local.Database.LocalUser.username
    |> Configuration.Url.user_key
    |> Uri.to_string in
  let priv_key =
    local.Database.LocalUser.privkey in
  log.debug (fun f ->
      f "sending accept follow request to %s\n\ndata is %s"
        uri (Yojson.Safe.pretty_to_string accept_follow)
    );

  let* resp, body  = Requests.signed_post (key_id, priv_key) (Uri.of_string uri)
      (Yojson.Safe.to_string accept_follow) in
  let* body = Cohttp_lwt.Body.to_string body >> Result.return in

  log.debug (fun f -> f  "response from server was %s" body);

  match resp.status with
  | `OK ->
    Database.Follows.update_pending_status
      ~id:(follow.Database.Follows.id) ~pending:false db
    |> sanitize
  | _ -> Lwt_result.fail "request failed"

let accept_local_follow _config follow ~target:remote ~author:local db =
  let* () =
    let* follow_remote = Database.Actor.resolve ~id:(follow.Database.Follows.target_id) db  |> sanitize
      >>= function
      | `Remote r ->
        let* r = Database.RemoteUser.resolve ~id:r db |> sanitize in
        Lwt.return_ok r.Database.RemoteUser.url
      | _ -> Lwt.return_error "invalid user" in
    if String.equal remote.Database.RemoteUser.url follow_remote
    then Lwt.return_ok ()
    else Lwt.return_error "inconsistent follow" in
  let* () =
    let* follow_local = Database.Actor.resolve ~id:(follow.Database.Follows.author_id) db |> sanitize
      >>= function
      | `Local l ->
        let* l = Database.LocalUser.resolve ~id:l db |> sanitize in
        Lwt.return_ok l.Database.LocalUser.username
      | _ -> Lwt.return_error "invalid user" in
    if String.equal (local.Database.LocalUser.username) follow_local
    then Lwt.return_ok ()
    else Lwt.return_error "inconsistent follow" in
  let* _ =
    Database.Follows.update_pending_status
      ~id:(follow.Database.Follows.id) ~pending:false db |> sanitize in
  Lwt_result.return ()

let follow_local_user follow_url remote_url local_user data db =
  let* remote = resolve_remote_user_by_url (Uri.of_string remote_url) db in
  let* follow = 
    let* author =
      Database.Actor.create_remote_user ~remote_id:(remote.Database.RemoteUser.id) db |> sanitize in
    let* target =
      Database.Actor.create_local_user ~local_id:(local_user.Database.LocalUser.id) db |> sanitize in
    Database.Follows.create
      ~raw_data:data
      ~url:follow_url ~author ~target ~pending:true
      ~created:(Ptime_clock.now ()) db |> sanitize in
  let* () =
    if not @@ local_user.Database.LocalUser.manually_accepts_follows
    then accept_remote_follow follow remote local_user db
    else Lwt_result.return () in
  Lwt_result.return ()

let create_note_request scope author to_ cc summary content content_type db =
  let post_id = fresh_id () in
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let published = Ptime_clock.now () in
  let* post =
    let* author =
      lift_database_error
        (Database.Actor.create_local_user ~local_id:(author.Database.LocalUser.id) db) in
    Database.Posts.create
      ~public_id:(post_id)
      ~url:(Configuration.Url.activity_endpoint
              post_id
            |> Uri.to_string)
      ~author ~is_public  ~is_follower_public
      ?summary
      ~post_source:content ~post_content:content_type
      ~published db
    |> lift_database_error in

  let* () = lift_database_error
      (Database.Posts.add_post_tos ~id:post.Database.Posts.id ~tos:to_ db) in
  let* () =
    lift_database_error
      (Database.Posts.add_post_ccs ~id:post.Database.Posts.id ~ccs:cc db) in
  let* to_ =
    lift_database_error (target_list_to_urls to_ db) in
  let* cc =
    lift_database_error (target_list_to_urls cc db) in

  let post_obj : Activitypub.Types.note =
    create_note_obj post_id author published scope to_ cc content summary in
  let data = Activitypub.Encode.note post_obj in
  let* _ =
    lift_database_error
      (Database.Activity.create ~id:post_id ~data db) in
  log.debug (fun f -> f "added post to database!");

  let create_post_id = fresh_id () in
  let create_note_obj : Activitypub.Types.note Activitypub.Types.create =
    create_create_event_obj create_post_id author published scope to_ cc post_obj in

  let data = Activitypub.Encode.(create note) create_note_obj in
  let* _ =
    lift_database_error
      (Database.Activity.create ~id:create_post_id ~data db) in

  Lwt.return_ok (Yojson.Safe.to_string data)

let create_new_note scope author to_ cc summary content content_type db =
  log.debug (fun f -> f "worker[create_new_note] ~author:%s ~to_:[%a] ~summary:%a ~content:%s"
                        author.Database.LocalUser.username (List.pp String.pp) to_
                        (Option.pp String.pp) summary content);
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let partition_user tagged_user =
    let* user = resolve_tagged_user tagged_user db in
    let* link =
      lift_database_error (match user with
          | `Local user -> Database.(Actor.create_local_user ~local_id:(user.LocalUser.id) db)
          | `Remote user -> Database.(Actor.create_remote_user ~remote_id:(user.RemoteUser.id) db)) in
    match user with
    | `Local _ -> return_ok (None, link)
    | `Remote r -> return_ok (Some r, link) in
  let suppress_errors = function
      Ok v -> Some v
    | Error err ->
      let _, msg, details = Error_handling.extract_error_details err in
      log.debug (fun f -> f "extracting target failed with error %s: %s" msg details);
      None in
  let* to_remotes, to_ =
    Lwt_list.map_s partition_user to_
    >> List.filter_map suppress_errors
    >> List.split
    >> Pair.map_fst (List.filter_map Fun.id)
    |> lift_pure in
  let* cc_remotes, cc =
    Lwt_list.map_s partition_user cc
    >> List.filter_map suppress_errors
    >> List.split
    >> Pair.map_fst (List.filter_map Fun.id)
    |> lift_pure in
  let* remote_followers_targets =
    if not (is_public || is_follower_public)
    then return_ok []
    else begin
      let* author =
        lift_database_error
          (Database.Actor.create_local_user ~local_id:(author.Database.LocalUser.id) db) in
      let* targets =
        lift_database_error
          (Database.Follows.collect_followers_for_actor ~id:author db) in
      let* targets =
        List.map (fun tgt -> tgt.Database.Follows.author_id) targets
        |> Lwt_list.map_s (fun v -> lift_database_error (Database.Actor.resolve ~id:v db))
        |> lift_pure in
      let remote_targets =
        List.filter_map suppress_errors targets
        |> List.filter_map (function `Remote r -> Some r | _ -> None) in
      let* remote_targets =
        Lwt_list.map_s (fun id -> lift_database_error (Database.RemoteUser.resolve ~id db))
          remote_targets
        >> List.all_ok in
      Lwt.return_ok remote_targets
    end in
  let* note_request =
    create_note_request scope author to_ cc summary content content_type db in
  let remote_targets = to_remotes @ cc_remotes @ remote_followers_targets in
  let* _ =
    let key_id =
      author.Database.LocalUser.username
      |> Configuration.Url.user_key
      |> Uri.to_string in
    let priv_key =
      author.Database.LocalUser.privkey in
    Lwt_list.map_p (fun r ->
        log.debug (fun f -> f "posting message to user %s" (r.Database.RemoteUser.username));
        let* remote_user_inbox =
          Lwt.return (Option.to_result (`WorkerFailure "no remote inbox") r.Database.RemoteUser.inbox) in
        log.debug (fun f -> f "inbox url %s" (remote_user_inbox));
        let* (response, body) =
          Requests.signed_post (key_id, priv_key)
            (Uri.of_string remote_user_inbox) note_request
          |> map_err (fun err -> `WorkerFailure err) in
        match Cohttp.Response.status response with
        | `OK ->
          let* _ = Cohttp_lwt.Body.drain_body body |> lift_pure in
          log.debug (fun f -> f "successfully sent message");
          return_ok ()
        | err ->
          let* body = Cohttp_lwt.Body.to_string body |> lift_pure in
          log.warning (fun f -> f "web post request failed with response %s; body %s"
                          (Cohttp.Code.string_of_status err)
                          body);
          return_ok ()
      ) remote_targets
    |> lift_pure in

  log.debug (fun f -> f "completed user's %s post" (author.Database.LocalUser.username));

  return_ok ()

let build_followers_collection_page start_time offset user db =
  let* followers, total_count =
    let* user =
      lift_database_error
        (Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db) in
    let* followers =
      lift_database_error
        (Database.Follows.collect_followers_for_actor
           ~offset:(offset * 10) ~limit:10 ~since:start_time ~id:user db) in
    let* total_count =
      lift_database_error
        (Database.Follows.count_followers ~target:user db) in
    Lwt.return_ok (followers, total_count) in
  let* followers =
    map_list (fun follow ->
        Database.Actor.resolve ~id:follow.Database.Follows.author_id db
      ) followers
    |> lift_database_error in
  let* followers =
    map_list (function
          `Local u ->
          let* u = Database.LocalUser.resolve ~id:u db
                   |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
          return_ok (Configuration.Url.user (u.Database.LocalUser.username)
                     |> Uri.to_string)
        | `Remote r ->
          let* r = Database.RemoteUser.resolve ~id:r db
                   |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
          return_ok r.Database.RemoteUser.url
      ) followers in
  let* start_time =
    (start_time
     |> Ptime.to_rfc3339 ~tz_offset_s:0
     |> return_ok) in
  let id =
    Configuration.Url.user_followers_page (user.Database.LocalUser.username)
      ~start_time ~offset:(Int.to_string offset)
    |> Uri.to_string in

  let next =
    Configuration.Url.user_followers_page (user.Database.LocalUser.username)
      ~start_time ~offset:(Int.to_string (offset + 1))
    |> Uri.to_string in

  let prev =
    Configuration.Url.user_followers_page (user.Database.LocalUser.username)
      ~start_time ~offset:(Int.to_string (offset - 1))
    |> Uri.to_string in

  let part_of =
    Configuration.Url.user_followers (user.Database.LocalUser.username)
    |> Uri.to_string in

  Lwt.return_ok ({
      id;
      prev=if offset > 0 then Some prev else None;
      next = if total_count < offset * 10 then None else Some next;
      is_ordered = true;
      items = followers;
      part_of=Some part_of;
      total_items=Some total_count;
    } : string Activitypub.Types.ordered_collection_page)

let build_following_collection_page start_time offset user db =
  let* following, total_count =
    let* user =
      lift_database_error
        (Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db) in
    let* following =
      lift_database_error
        (Database.Follows.collect_following_for_actor
           ~since:start_time ~limit:10 ~offset:(offset * 10) ~id:user db) in
    let* total_count =
      lift_database_error
        (Database.Follows.count_following ~author:user db) in
    Lwt.return_ok (following, total_count) in
  let* following =
    map_list (fun follow ->
        Database.Actor.resolve ~id:follow.Database.Follows.author_id db
      ) following
    |> lift_database_error in
  let* following =
    map_list (function
          `Local u ->
          let* u = Database.LocalUser.resolve ~id:u db in
          return_ok (Configuration.Url.user (u.Database.LocalUser.username)
                     |> Uri.to_string)
        | `Remote r ->
          let* r = Database.RemoteUser.resolve ~id:r db in
          return_ok r.Database.RemoteUser.url
      ) following
    |> lift_database_error in
  let* start_time =
    (start_time
     |> Ptime.to_rfc3339 ~tz_offset_s:0
     |> Lwt.return_ok) in
  let id =
    Configuration.Url.user_following_page (user.Database.LocalUser.username)
      ~start_time ~offset:(Int.to_string offset)
    |> Uri.to_string in

  let next =
    Configuration.Url.user_following_page (user.Database.LocalUser.username)
      ~start_time ~offset:(Int.to_string (offset + 1))
    |> Uri.to_string in

  let prev =
    Configuration.Url.user_following_page (user.Database.LocalUser.username)
      ~start_time ~offset:(Int.to_string (offset - 1))
    |> Uri.to_string in

  let part_of =
    Configuration.Url.user_following (user.Database.LocalUser.username)
    |> Uri.to_string in

  Lwt.return_ok ({
      id;
      prev=if offset > 0 then Some prev else None;
      next = if total_count < offset * 10 then None else Some next;
      is_ordered = true;
      items = following;
      part_of=Some part_of;
      total_items=Some total_count;
    } : string Activitypub.Types.ordered_collection_page)
 
