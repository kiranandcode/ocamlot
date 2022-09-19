[@@@warning "-33-27"]
open Containers
open Common

let log = Logging.add_logger "worker"

(* let (let+!) x f = Lwt_result.bind (sanitize x) f *)

type task =
  (* | CreateNote of {
   *   id: string;
   *   author: string;
   *   to_: string list; cc: string list;
   *   sensitive: bool; direct_message: bool;
   *   content: string; source: string option; summary: string option;
   *   published: Ptime.t option;
   *   tags: [ `Raw of Yojson.Safe.t | `Value of Activitypub.Types.tag ] list;
   *   data: Yojson.Safe.t;
   * }
   * (\* follow by a local user *\)
   * | LocalFollow of {
   *   local: Database.LocalUser.t;   (\* local user doing the following  *\)
   *   username: string;              (\* remote username being followed  *\)
   *   domain: string;                (\* remote domain being followed *\)
   * }
   * | RecordAcceptLocalFollow of {
   *     follow: Database.Follow.t;     (\* follow being accepted *\)
   *     author: Database.LocalUser.t;  (\* local user who made the request accepted *\)
   *     target: Database.RemoteUser.t; (\* url of remote user doing the accepting *\)
   *   }
   * 
   * (\* follow by remote user *\)
   * | RemoteFollow of {
   *   id: string;                    (\* url of the follow object *\)
   *   remote: string;                (\* url of remote actor making the follow *\)
   *   target: Database.LocalUser.t;  (\* url of local actor being followed *\)
   *   data: Yojson.Safe.t;           (\* raw data of the follow object *\)
   * }
   * | AcceptRemoteFollow of {
   *     follow: Database.Follow.t;
   *     author: Database.RemoteUser.t;
   *     target: Database.LocalUser.t;
   *   } *)

  (* post by local user *)
  | LocalPost of {
    user: Database.LocalUser.t;
    title: string option;
    content_type: [ `Markdown | `Org | `Text ];
    scope: [ `DM | `Followers | `Public ];
    post_to: string list option;
    content: string;
  }

open struct

  let with_pool pool f = Caqti_lwt.Pool.use f pool

  let handle_error res =
    Lwt.bind res (function
      | Ok _ -> Lwt.return ()
      | Error (#Caqti_error.t as err) ->
        let _, msg, details = Error_handling.extract_error_details (`DatabaseError (Caqti_error.show err)) in
        (* log.warning (fun f -> f "worker error: %s" msg); *)
        Format.printf "worker error: %s" msg;
        (* log.debug (fun f -> f "worker error details: %s" details); *)
        Format.printf "worker error details: %s" details;
        Lwt.return ()
      | Error err ->
        let _, msg, details = Error_handling.extract_error_details err in
        (* log.warning (fun f -> f "worker error: %s" msg); *)
        Format.printf "worker error: %s" msg;
        (* log.debug (fun f -> f "worker error details: %s" details); *)
        Format.printf "worker error details: %s" details;

        Lwt.return ()
    )


  let extract_user config db user = 
    log.debug (fun f -> f "line 81: extract user");
    let user_tag = Configuration.Regex.user_tag config |> Re.compile in
    log.debug (fun f -> f "line 83: user tag");
    let matches = Re.all user_tag (String.trim user) in
    log.debug (fun f -> f "line 85: matches");
    let user_tag = List.head_opt matches in
    log.debug (fun f -> f "line 87: user tag");
    match user_tag with
    | None ->
      (* remote user by url *)
      log.debug (fun f -> f "line 91: none user tag");
      let+ remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string user) db
                         |> map_err (fun err -> `WorkerFailure err ) in
      return (Ok (`Remote remote_user))
    | Some group when Option.equal String.equal (Some (Configuration.Params.domain config |> Uri.to_string)) (Re.Group.get_opt group 2) ->
      log.debug (fun f -> f "line 96: some group user tag");
      (* local user *)
      let username = Re.Group.get group 1 in
      log.debug (fun f -> f "line 91: username");
      let+ resolved_user = Database.LocalUser.lookup_user ~username db |> map_err (fun msg -> `DatabaseError msg) in
      log.debug (fun f -> f "line 91: resolved user");
      begin match resolved_user with
      | None -> return (Error (`WorkerFailure (Format.sprintf "could not resolve local user %s" user)))
      | Some user -> return_ok (`Local user)
      end
    | Some group ->
      log.debug (fun f -> f "line 107: some group not local");
      let username = Re.Group.get group 1 in
      log.debug (fun f -> f "line 107: username is %s" username);
      let domain = Re.Group.get group 2 in
      log.debug (fun f -> f "line 107: domain is %s" domain);
      let+ resolved_user = Resolver.resolve_remote_user ~username ~domain db
                           |> map_err (fun msg -> `WorkerFailure msg) in
      log.debug (fun f -> f "line 114: resolved user!");
      return_ok (`Remote resolved_user)


  let handle_local_post pool config user scope post_to title content_type content =
    log.debug (fun f -> f "working on local post by %s of %s" (Database.LocalUser.username user) content);
    let id = Database.Activity.fresh_id () in

    let is_public, is_follower_public =
      match scope with
      | `DM -> false, false
      | `Followers -> false, true
      | `Public -> true, true in

    let+ post =
      with_pool pool @@ fun db ->
      (* lookup the author *)
      let+ author = (Database.Actor.of_local (Database.LocalUser.self user) db)
                    |> map_err (fun err -> `DatabaseError err) in
      log.debug (fun f -> f "retreived user:--> %s" (Database.LocalUser.username user));

      Database.Post.create_post
        ~public_id:(Database.Activity.id_to_string id)
        ~url:(Configuration.Url.activity_endpoint config
                (Database.Activity.id_to_string id)
              |> Uri.to_string)
        ~author ~is_public  ~is_follower_public
        ?summary:title
        ~post_source:content ~post_content:content_type
        ~published:(CalendarLib.Calendar.now ()) db
      |> map_err (fun err -> `DatabaseError err) in
    log.debug (fun f -> f "added post to database!");
    let+ remote_targets =
      let post_link = Database.Post.self post in
      log.debug (fun f -> f "line 136: got post link");
      with_pool pool @@ fun db ->
      log.debug (fun f -> f "line 138: got pool");
      Lwt_list.map_p (fun tagged_user ->
        log.debug (fun f -> f "line 140: tagged user");
        let+ user = extract_user config db tagged_user in
        log.debug (fun f -> f "line 142: user");
        let+ user_link = begin match user with
          | `Local user -> Database.(Actor.of_local (LocalUser.self user) db)
          | `Remote user -> Database.(Actor.of_remote (RemoteUser.self user) db)
        end |> map_err (fun msg -> `DatabaseError msg) in
        log.debug (fun f -> f "line 147: user");
        let+ () = Database.Post.add_post_to post_link user_link db
                  |> map_err (fun msg -> `WorkerFailure msg) in
        match user with
        | `Remote user -> return_ok (Some user)
        | _ -> return_ok None
      ) (Option.value ~default:[] post_to) |> lift_pure in
    log.debug (fun f -> f "collected initial remote targets!");
    (* collect remote direct targets *)
    let remote_direct_targets =
      List.filter_map (function
        | Ok v -> v
        | Error error ->
          let _, title, details = Error_handling.extract_error_details error in
          log.info (fun f -> f "Resolving targets for post failed with message %s - %s" title details);
          None
      ) remote_targets in
    log.debug (fun f -> f "collected direct remote targets!");
    (* if post is to followers/public, then add all remote followers *)
    let+ remote_follower_targets =
      if not (is_public || is_follower_public)
      then return_ok []
      else begin
        with_pool pool @@ fun db ->
        let+ author = (Database.Actor.of_local (Database.LocalUser.self user) db)
                      |> map_err (fun err -> `DatabaseError err) in
        let+ targets = Database.Follow.collect_followers author db
                       |> map_err (fun err -> `DatabaseError err) in
        let+ targets =
          List.map Database.Follow.author targets
          |> Lwt_list.map_p (fun v -> Database.Link.resolve v db)
          |> lift_pure in
        let remote_targets =
          List.filter_map (function
            | Ok Database.Actor.Remote r ->
              Some r
            | Ok _ -> None
            | Error msg -> 
              let _, title, details = Error_handling.extract_error_details (`DatabaseError msg) in
              log.info (fun f ->
                f "Resolving followers for post failed with message %s - %s" title details);
              None
          ) targets in
        Lwt.return_ok remote_targets
      end in
    (* collect the list of targets to send the post to *)
    let remote_targets = remote_direct_targets @ remote_follower_targets in
    log.debug (fun f -> f "found %d remote targets for the message" (List.length remote_targets));

    let+ _ =
      let post_content =
        ""
      in
      let key_id =
        Database.LocalUser.username user
        |> Configuration.Url.user_key config
        |> Uri.to_string in
      let priv_key =
        Database.LocalUser.privkey user in
      Lwt_list.map_p (fun r ->
        log.debug (fun f -> f "posting message to user %s" (Database.RemoteUser.username r));
        let remote_user_inbox = Database.RemoteUser.inbox r in
        log.debug (fun f -> f "inbox url %s" (Uri.to_string remote_user_inbox));
        let+ (response, body) = Resolver.signed_post (key_id, priv_key) remote_user_inbox post_content
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

    log.debug (fun f -> f "completed user's %s post" (Database.LocalUser.username user));
    Lwt.return_ok ()

  let worker (pool: (Caqti_lwt.connection, [> Caqti_error.t]) Caqti_lwt.Pool.t) task_in config =
    log.debug (fun f -> f "worker now waiting for task");
    Lwt_stream.iter_s 
      (fun task ->
         log.debug (fun f -> f "worker woken up with task");
         try
           begin match task with
           | LocalPost {user; title; content; content_type; scope; post_to;} -> 
             let res = handle_local_post pool config user scope post_to title content_type content in
             handle_error res
           end |> Lwt.map ignore
         with
         | exn ->
           log.debug (fun f -> f "worker failed with exn %s" (Printexc.to_string exn));
           Lwt.return ()
      )
      task_in

end


let init config task_in =
  (* hackity hack hacks to get access to Dream's internal Sql pool *)
  let pool =
    let vl = ref None in
    ignore @@ Dream.sql_pool  Configuration.Params.(database_path config) (fun req ->
      vl :=(Dream__sql.Sql.Message.field req Dream__sql.Sql.pool_field);
      Dream.html ""
    ) (Dream.request "");
    match !vl with
    | None -> raise (Failure "worker failed to acquire access to pool")
    | Some pool -> pool  in
  (* configure journal with WAL and busy timeout to avoid busy errors *)
  log.info (fun f -> f "setting up worker database configurations");
  Lwt.join [
    worker (Obj.magic pool) task_in config;
    let enable_journal_mode =
      Caqti_request.Infix.(Caqti_type.unit -->! Caqti_type.string @:-  "PRAGMA journal_mode=WAL" ) in
    let update_busy_timout =
      Caqti_request.Infix.(Caqti_type.unit -->! Caqti_type.int @:-  "PRAGMA busy_timeout = 5000" ) in
    ((Caqti_lwt.Pool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
       let+ _ = DB.find enable_journal_mode () in
       let+ _ = DB.find update_busy_timout () in
       Lwt.return_ok ()
     ) pool)
     |> Obj.magic
     |> handle_error)
  ]

