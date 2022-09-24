[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "back.resolver"

let req_post ~headers url body =
  let body = Cohttp_lwt.Body.of_string body in
  try
    Cohttp_lwt_unix.Client.post
      ~headers
      ~body
      url >> Result.return
  with exn ->
    Lwt.return (Result.of_exn exn)

let req ~headers url =
  try
    Cohttp_lwt_unix.Client.get
      ~headers:(Cohttp.Header.of_list headers)
      url
    >> Result.return 
  with exn -> Lwt.return (Result.of_exn exn)

let signed_req f (key_id, priv_key) uri body_str =
  let current_time = Ptime_clock.now () in
  let headers =
    Http_sig.build_signed_headers
      ~current_time ~method_:"POST" ~body_str
      ~headers:(Http_sig.StringMap.of_list [
        "Content-Type", APConstants.ContentType.ld_json_activity_streams
      ]) ~key_id ~priv_key ~uri
    |> Cohttp.Header.of_list in
  f ~headers uri body_str

let signed_post key uri body =
  signed_req req_post key uri body

let activity_req ?(headers=[]) url =
  let activity_header =
    ("Accept", APConstants.ContentType.activity_json) in
  req ~headers:(activity_header :: headers) url

let json_rd_req ?(headers=[]) url =
  let json_rd_header =
    ("Accept", APConstants.Webfinger.json_rd) in
  req ~headers:(json_rd_header :: headers) url

let resolve_public_key url =
  (* NOTE: Not obvious, but you need to specify accept headers, else pleroma will return html *)
  let+ (_resp, body) = activity_req (Uri.of_string url) in
  let+ actor = Cohttp_lwt.Body.to_string body
               |> Lwt.map Activitypub.Decode.(decode_string person) in
  let pub_key =
    actor.public_key.pem
    |> Cstruct.of_string
    |> X509.Public_key.decode_pem
    |> Result.map_err (fun (`Msg err) -> err) in
  Lwt.return pub_key

let resolve_remote_user_with_webfinger ~local_lookup ~webfinger_uri db
  : (Database.RemoteUser.t, string) Lwt_result.t =
  let+ domain = Uri.host webfinger_uri |> Result.of_opt |> Lwt.return in
  let extract_self_link query =
    query.Activitypub.Types.Webfinger.links
    |> List.find_map (function
        Activitypub.Types.Webfinger.Self (
          (`ActivityJson | `ActivityJsonLd | `Json), url
        ) -> Some (Uri.of_string url)
      | _ -> None)
    |> Result.of_opt
    |> Lwt.return in
  let+ result = local_lookup db in
  match result with
    Some v -> Lwt.return_ok v
  | None ->
    (* remote user not found *)
    (* webfinger to find user url *)
    let+ remote_user_url =
      let+ (_, body) = json_rd_req webfinger_uri in
      let+ body = Cohttp_lwt.Body.to_string body >> Result.return in
      let+ query_res = body
                       |> Activitypub.Decode.(decode_string Webfinger.query_result)
                       |> Lwt.return in
      extract_self_link query_res in
    (* retrieve json *)
    let+ (_, body) = activity_req remote_user_url in
    let+ body = Cohttp_lwt.Body.to_string body >> Result.return in
    let+ person_res = body
                      |> Activitypub.Decode.(decode_string person)
                      |> Lwt.return in
    let+ remote_instance = Database.RemoteInstance.create_instance domain db in
    let+ () = Database.RemoteInstance.record_instance_reachable remote_instance db in
    let+ username = person_res.preferred_username
                    |> Result.of_opt
                    |> Lwt.return in
    let+ url = person_res.url
               |> Result.of_opt
               |> Lwt.return in
    Database.RemoteUser.create_remote_user
      ?display_name:person_res.name
      ~inbox:person_res.inbox
      ~outbox:person_res.outbox
      ?followers:person_res.followers
      ?following:person_res.following
      ?summary:person_res.summary
      ~public_key_pem:person_res.public_key.pem
      ~username
      ~instance:(Database.RemoteInstance.self remote_instance)
      ~url:url db

let resolve_remote_user ~username ~domain db =
  resolve_remote_user_with_webfinger
    ~local_lookup:(Database.RemoteUser.lookup_remote_user_by_address ~username ~domain)
    ~webfinger_uri:(Uri.make
                      ~scheme:"https"
                      ~host:domain
                      ~path:"/.well-known/webfinger"
                      ~query:["resource", [Printf.sprintf "acct:%s@%s" username domain]] ()
                   ) db

let resolve_remote_user_by_url url db =
  let url' = Uri.to_string url in
  resolve_remote_user_with_webfinger
    ~local_lookup:(Database.RemoteUser.lookup_remote_user_by_url url')
    ~webfinger_uri:(
      url
      |> Fun.flip Uri.with_path "/.well-known/webfinger"
      |> Fun.flip Uri.with_query' ["resource", url']
      |> Fun.flip Uri.with_scheme (Some "https")
    ) db


(** [resolve_tagged_user config username db] given a [username] in the
    form {<username>@<domain>} classifies the user a a local or remote
    user if they exist. *)
let resolve_tagged_user config user db = 
  let user_tag = Configuration.Regex.user_tag config |> Re.compile in
  let matches = Re.all user_tag (String.trim user) in
  let user_tag = List.head_opt matches in
  match user_tag with
  | None ->
    (* remote user by url *)
    let+ remote_user = resolve_remote_user_by_url (Uri.of_string user) db
                       |> map_err (fun err -> `WorkerFailure err ) in
    return (Ok (`Remote remote_user))
  | Some group when Option.equal String.equal
                      (Some (Configuration.Params.domain config |> Uri.to_string))
                      (Re.Group.get_opt group 2) ->
    (* local user *)
    let username = Re.Group.get group 1 in
    let+ resolved_user = Database.LocalUser.lookup_user ~username db |> map_err (fun msg -> `DatabaseError msg) in
    begin match resolved_user with
    | None -> return (Error (`WorkerFailure (Format.sprintf "could not resolve local user %s" user)))
    | Some user -> return_ok (`Local user)
    end
  | Some group ->
    let username = Re.Group.get group 1 in
    let domain = Re.Group.get group 2 in
    let+ resolved_user = resolve_remote_user ~username ~domain db
                         |> map_err (fun msg -> `WorkerFailure msg) in
    return_ok (`Remote resolved_user)

let create_accept_follow config follow remote local db =
  let local_user =
    Configuration.Url.user config (Database.LocalUser.username local)
    |> Uri.to_string in
  let id = Database.Activity.fresh_id () in
  let accept = 
    ({
      id=Configuration.Url.activity_endpoint config (Database.Activity.id_to_string id) |> Uri.to_string;
      actor=local_user;
      published=Some (Ptime_clock.now ());
      obj=({
        id=Database.Follow.url follow;
        actor=Database.RemoteUser.url remote;
        cc=[]; to_=[local_user]; state=None; raw=`Null;
        object_=local_user;
      }: Activitypub.Types.follow); raw=`Null;
    }:_ Activitypub.Types.accept) in
  let accept = Activitypub.Encode.(accept follow) accept in
  let+ _ = Database.Activity.create ~id ~data:accept db in
  Lwt_result.return accept

let target_list_to_urls config to_ db =
  Lwt_list.map_s (fun actor ->
    Database.Link.resolve actor db
    >> Result.map (function
      | Database.Actor.Local l ->
        Database.LocalUser.username l
        |> Configuration.Url.user config
        |> Uri.to_string
      | Database.Actor.Remote r ->
        Database.RemoteUser.url r)
    |> map_err (fun err -> `DatabaseError err)
  ) to_
  >> List.filter_map (function
    | Ok v -> Some v
    | Error err ->
      let _, msg, details = Error_handling.extract_error_details err in
      log.debug (fun f -> f "converting target to url failed with %s: %s" msg details);
      None
  )
  |> lift_pure

let create_note_obj config post_id author published scope to_ cc content summary =
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let id =
    Database.Activity.id_to_string post_id
    |> Configuration.Url.activity_endpoint config
    |> Uri.to_string in
  let actor = 
    Database.LocalUser.username author
    |> Configuration.Url.user config
    |> Uri.to_string in
  let published =
    published
    |> CalendarLib.Calendar.to_unixfloat
    |> Ptime.of_float_s
    |> Option.get_exn_or "" in
  let to_ =
    if is_public
    then (Activitypub.Constants.ActivityStreams.public :: to_)
    else to_ in
  let cc = if is_follower_public
    then
      let followers_url =
        Database.LocalUser.username author
        |> Configuration.Url.user_followers config
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

let create_create_event_obj config post_id author published scope to_ cc post =
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let id =
    Database.Activity.id_to_string post_id
    |> Configuration.Url.activity_endpoint config
    |> Uri.to_string in
  let actor = 
    Database.LocalUser.username author
    |> Configuration.Url.user config
    |> Uri.to_string in
  let published =
    published
    |> CalendarLib.Calendar.to_unixfloat
    |> Ptime.of_float_s
    |> Option.get_exn_or "" in
  let to_ =
    if is_public
    then (Activitypub.Constants.ActivityStreams.public :: to_)
    else to_ in
  let cc = if is_follower_public
    then
      let followers_url =
        Database.LocalUser.username author
        |> Configuration.Url.user_followers config
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

let build_follow_request config local remote db =
  log.debug (fun f -> f "building follow request");
  let id = try Database.Activity.fresh_id () with exn ->
    log.debug (fun f -> f "failed with error %s" (Printexc.to_string exn));
    raise exn in
  log.debug (fun f -> f "created fresh id %s" (Database.Activity.id_to_string id));
  let local_actor_url = 
    Database.LocalUser.username local
    |> Configuration.Url.user config
    |> Uri.to_string in
  log.debug (fun f -> f "calculated local actor url %s" local_actor_url);
  let remote_actor_url = Database.RemoteUser.url remote in
  log.debug (fun f -> f "calculated remote actor url %s" remote_actor_url);
  let follow_request =
    let id =
      Database.Activity.id_to_string id
      |> Configuration.Url.activity_endpoint config
      |> Uri.to_string in
    Activitypub.Types.{
      id; actor=local_actor_url;
      cc = []; to_ = [ remote_actor_url ];
      object_=remote_actor_url; state = Some `Pending;
      raw=`Null
    }  in
  let data = Activitypub.Encode.follow follow_request in
  log.debug (fun f -> f "constructed yojson object %a" Yojson.Safe.pp data);
  let+ _ =
    log.debug (fun f -> f "resolving remote author");
    let+ author = Database.Actor.of_local (Database.LocalUser.self local) db in
    log.debug (fun f -> f "resolving target");
    let+ target = Database.Actor.of_remote (Database.RemoteUser.self remote) db in
    log.debug (fun f -> f "creating follow");
    Database.Follow.create_follow
      ~url:(Configuration.Url.activity_endpoint config (Database.Activity.id_to_string id)
            |> Uri.to_string)
      ~public_id:(Database.Activity.id_to_string id)
      ~author ~target ~pending:true
      ~created:(CalendarLib.Calendar.now ()) db in
  log.debug (fun f -> f "creating follow in activity db");
  let+ _ = Database.Activity.create ~id ~data db in
  Lwt_result.return (data |> Yojson.Safe.to_string)

let follow_remote_user config
      (local: Database.LocalUser.t)
      ~username ~domain db: (unit,string) Lwt_result.t =
  log.debug (fun f -> f "resolving remote user %s@%s" username domain);
  let+ remote = resolve_remote_user ~username ~domain db in
  log.debug (fun f -> f "successfully resolved remote user %s@%s" username domain);
  let+ follow_request = build_follow_request config local remote db in
  log.debug (fun f -> f "built follow request %s" follow_request);
  let uri = Database.RemoteUser.inbox remote in
  let key_id =
    Database.LocalUser.username local
    |> Configuration.Url.user_key config
    |> Uri.to_string in
  let priv_key =
    Database.LocalUser.privkey local in
  log.debug (fun f -> f "sending signed follow request");
  let+ resp, body  = signed_post (key_id, priv_key) uri follow_request in
  let+ body = lift_pure (Cohttp_lwt.Body.to_string body) in
  log.debug (fun f -> f "follow request response was (STATUS: %s) %s" (Cohttp.Code.string_of_status resp.status) body);
  match resp.status with
  | `OK -> Lwt_result.return ()
  | _ -> Lwt_result.fail "request failed"

let accept_remote_follow config follow remote local db =
  let+ accept_follow = create_accept_follow config follow remote local db in
  let uri = Database.RemoteUser.inbox remote in
  let key_id =
    Database.LocalUser.username local
    |> Configuration.Url.user_key config
    |> Uri.to_string in
  let priv_key =
    Database.LocalUser.privkey local in
  log.debug (fun f ->
    f "sending accept follow request to %s\n\ndata is %s"
      (Uri.to_string uri) (Yojson.Safe.pretty_to_string accept_follow)
  );

  let+ resp, body  = signed_post (key_id, priv_key) uri
                       (Yojson.Safe.to_string accept_follow) in
  let+ body = Cohttp_lwt.Body.to_string body >> Result.return in

  log.debug (fun f -> f  "response from server was %s" body);

  match resp.status with
  | `OK ->
    let+ () =
      Database.Follow.update_follow_pending_status ~timestamp:(CalendarLib.Calendar.now ())
        (Database.Follow.self follow) false db in
    Lwt_result.return ()
  | _ -> Lwt_result.fail "request failed"

let accept_local_follow _config follow ~target:remote ~author:local db =
  let+ () =
    let+ follow_remote = Database.Link.resolve (Database.Follow.target follow) db
      >>= function Database.Actor.Remote r -> Lwt.return_ok (Database.RemoteUser.url r)
                 | _ -> Lwt.return_error "invalid user" in
    if String.equal (Database.RemoteUser.url remote) follow_remote
    then Lwt.return_ok ()
    else Lwt.return_error "inconsistent follow" in
  let+ () =
    let+ follow_local = Database.Link.resolve (Database.Follow.author follow) db
      >>= function Database.Actor.Local l -> Lwt.return_ok (Database.LocalUser.username l)
                 | _ -> Lwt.return_error "invalid user" in
    if String.equal (Database.LocalUser.username local) follow_local
    then Lwt.return_ok ()
    else Lwt.return_error "inconsistent follow" in
  let+ _ =
    Database.Follow.update_follow_pending_status ~timestamp:(CalendarLib.Calendar.now ())
      (Database.Follow.self follow) false db in
  Lwt_result.return ()


let follow_local_user config follow_url remote_url local_user data db =
  let+ remote = resolve_remote_user_by_url (Uri.of_string remote_url) db in
  let+ follow = 
    let+ author =
      Database.Actor.of_remote (Database.RemoteUser.self remote) db in
    let+ target =
      Database.Actor.of_local (Database.LocalUser.self local_user) db in
    Database.Follow.create_follow
      ~raw_data:(Yojson.Safe.to_string data)
      ~url:follow_url ~author ~target ~pending:true
      ~created:(CalendarLib.Calendar.now ()) db in
  let+ () =
    if not @@ Database.LocalUser.manually_accept_follows local_user
    then accept_remote_follow config follow remote local_user db
    else Lwt_result.return () in
  Lwt_result.return ()

let create_note_request config scope author to_ cc summary content content_type db =
  let post_id = Database.Activity.fresh_id () in
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let published = CalendarLib.Calendar.now () in
  let+ post =
    let+ author = (Database.Actor.of_local (Database.LocalUser.self author) db)
                  |> map_err (fun err -> `DatabaseError err) in
    Database.Post.create_post
      ~public_id:(Database.Activity.id_to_string post_id)
      ~url:(Configuration.Url.activity_endpoint config
              (Database.Activity.id_to_string post_id)
            |> Uri.to_string)
      ~author ~is_public  ~is_follower_public
      ?summary
      ~post_source:content ~post_content:content_type
      ~published db
    |> map_err (fun err -> `DatabaseError err) in
  log.debug (fun f -> f "added post to database!");
  let+ () = Database.Post.add_post_tos Database.Post.(self post) to_ db
            |> map_err (fun err -> `DatabaseError err) in
  let+ () = Database.Post.add_post_ccs Database.Post.(self post) cc db
            |> map_err (fun err -> `DatabaseError err) in
  let+ to_ = target_list_to_urls config to_ db in
  let+ cc = target_list_to_urls config cc db in

  let post_obj : Activitypub.Types.note =
    create_note_obj config post_id author published scope to_ cc content summary in

  let create_post_id = Database.Activity.fresh_id () in
  let create_note_obj : Activitypub.Types.note Activitypub.Types.create =
    create_create_event_obj config create_post_id author published scope to_ cc post_obj in

  let data = Activitypub.Encode.(create note) create_note_obj in
  let+ _ = Database.Activity.create ~id:create_post_id ~data db
           |> map_err (fun err -> `DatabaseError err) in

  Lwt.return_ok (Yojson.Safe.to_string data)

let create_new_note config scope author to_ cc summary content content_type db =
  let is_public, is_follower_public =
    match scope with
    | `DM -> false, false
    | `Followers -> false, true
    | `Public -> true, true in
  let partition_user tagged_user =
    let+ user = resolve_tagged_user config tagged_user db in
    let+ link = (match user with
      | `Local user -> Database.(Actor.of_local (LocalUser.self user) db)
      | `Remote user -> Database.(Actor.of_remote (RemoteUser.self user) db))
                |> map_err (fun err -> `DatabaseError err) in
    match user with
    | `Local _ -> return_ok (None, link)
    | `Remote r -> return_ok (Some r, link) in
  let suppress_errors = function
      Ok v -> Some v
    | Error err ->
      let _, msg, details = Error_handling.extract_error_details err in
      log.debug (fun f -> f "extracting target failed with error %s: %s" msg details);
      None in
  let+ to_remotes, to_ =
    Lwt_list.map_s partition_user to_
    >> List.filter_map suppress_errors
    >> List.split
    >> Pair.map_fst (List.filter_map Fun.id)
    |> lift_pure in
  let+ cc_remotes, cc =
    Lwt_list.map_s partition_user cc
    >> List.filter_map suppress_errors
    >> List.split
    >> Pair.map_fst (List.filter_map Fun.id)
    |> lift_pure in
  let+ remote_followers_targets =
    if not (is_public || is_follower_public)
    then return_ok []
    else begin
      let+ author = (Database.Actor.of_local (Database.LocalUser.self author) db)
                    |> map_err (fun err -> `DatabaseError err) in
      let+ targets = Database.Follow.collect_followers author db
                     |> map_err (fun err -> `DatabaseError err) in
      let+ targets =
        List.map Database.Follow.author targets
        |> Lwt_list.map_s (fun v -> Database.Link.resolve v db |> map_err (fun err -> `DatabaseError err))
        |> lift_pure in
      let remote_targets =
        List.filter_map suppress_errors targets
        |> List.filter_map (function Database.Actor.Remote r -> Some r | _ -> None) in
      Lwt.return_ok remote_targets
    end in
  let+ note_request =
    create_note_request config scope author to_ cc summary content content_type db in
  let remote_targets = to_remotes @ cc_remotes @ remote_followers_targets in
  let+ _ =
    let key_id =
      Database.LocalUser.username author
      |> Configuration.Url.user_key config
      |> Uri.to_string in
    let priv_key =
      Database.LocalUser.privkey author in
    Lwt_list.map_p (fun r ->
      log.debug (fun f -> f "posting message to user %s" (Database.RemoteUser.username r));
      let remote_user_inbox = Database.RemoteUser.inbox r in
      log.debug (fun f -> f "inbox url %s" (Uri.to_string remote_user_inbox));
      let+ (response, body) = signed_post (key_id, priv_key) remote_user_inbox note_request
                              |> map_err (fun err -> `WorkerFailure err) in
      match Cohttp.Response.status response with
      | `OK ->
        let+ _ = Cohttp_lwt.Body.drain_body body |> lift_pure in
        log.debug (fun f -> f "successfully sent message");
        return_ok ()
      | err ->
        let+ body = Cohttp_lwt.Body.to_string body |> lift_pure in
        log.warning (fun f -> f "web post request failed with response %s; body %s"
                                (Cohttp.Code.string_of_status err)
                                body);
        return_ok ()
    ) remote_targets
    |> lift_pure in

  log.debug (fun f -> f "completed user's %s post" (Database.LocalUser.username author));

  return_ok ()



let build_followers_collection_page config start_time offset user db =
  let+ followers, total_count =
    let+ user = Database.Actor.of_local (Database.LocalUser.self user) db in
    let+ followers =
      Database.Follow.collect_followers
        ~offset:(start_time, 10, offset * 10) user db in
    let+ total_count =
      Database.Follow.count_followers user db in
    Lwt.return_ok (followers, total_count) in

  let+ followers =
    Lwt_list.map_s (fun follow ->
      Database.Follow.author follow
      |> Fun.flip Database.Link.resolve db
    ) followers
    >> Result.flatten_l in
  let followers =
    List.map (function
        Database.Actor.Local u ->
        Configuration.Url.user config (Database.LocalUser.username u)
        |> Uri.to_string
      | Database.Actor.Remote r ->
        Database.RemoteUser.url r
    ) followers in
  let+ start_time =
    (CalendarLib.Calendar.to_unixfloat start_time
     |> Ptime.of_float_s
     |> Option.map (Ptime.to_rfc3339 ~tz_offset_s:0)
     |> Result.of_opt
     |> Lwt.return) in
  let id =
    Configuration.Url.user_followers_page config (Database.LocalUser.username user)
      ~start_time ~offset:(Int.to_string offset)
    |> Uri.to_string in

  let next =
    Configuration.Url.user_followers_page config (Database.LocalUser.username user)
      ~start_time ~offset:(Int.to_string (offset + 1))
    |> Uri.to_string in

  let prev =
    Configuration.Url.user_followers_page config (Database.LocalUser.username user)
      ~start_time ~offset:(Int.to_string (offset - 1))
    |> Uri.to_string in

  let part_of =
    Configuration.Url.user_followers config (Database.LocalUser.username user)
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

let build_following_collection_page config start_time offset user db =
  let+ following, total_count =
    let+ user = Database.Actor.of_local (Database.LocalUser.self user) db in
    let+ following =
      Database.Follow.collect_following
        ~offset:(start_time, 10, offset * 10) user db in
    let+ total_count =
      Database.Follow.count_following user db in
    Lwt.return_ok (following, total_count) in

  let+ following =
    Lwt_list.map_s (fun follow ->
      Database.Follow.author follow
      |> Fun.flip Database.Link.resolve db
    ) following
    >> Result.flatten_l in
  let following =
    List.map (function
        Database.Actor.Local u ->
        Configuration.Url.user config (Database.LocalUser.username u)
        |> Uri.to_string
      | Database.Actor.Remote r ->
        Database.RemoteUser.url r
    ) following in
  let+ start_time =
    (CalendarLib.Calendar.to_unixfloat start_time
     |> Ptime.of_float_s
     |> Option.map (Ptime.to_rfc3339 ~tz_offset_s:0)
     |> Result.of_opt
     |> Lwt.return) in
  let id =
    Configuration.Url.user_following_page config (Database.LocalUser.username user)
      ~start_time ~offset:(Int.to_string offset)
    |> Uri.to_string in

  let next =
    Configuration.Url.user_following_page config (Database.LocalUser.username user)
      ~start_time ~offset:(Int.to_string (offset + 1))
    |> Uri.to_string in

  let prev =
    Configuration.Url.user_following_page config (Database.LocalUser.username user)
      ~start_time ~offset:(Int.to_string (offset - 1))
    |> Uri.to_string in

  let part_of =
    Configuration.Url.user_following config (Database.LocalUser.username user)
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
