[@@@warning "-33-27"]
open Containers
open Common

let log = Logging.add_logger "worker"

type task =
  | CreateRemoteNote of {
    author : Database.RemoteUser.t;
    direct_message: bool;
    note: Activitypub.Types.note;
  }
  | HandleUndoFollow of {
    follow_id: string;
  }

  | HandleRemoteFollow of {
    id: string;
    actor: string;
    target: Database.LocalUser.t;
    raw: Yojson.Safe.t;
  }

  | HandleAcceptFollow of { follow_id: string; }

  | FollowRemoteUser of {
    user: Database.LocalUser.t;
    username: string;
    domain: string;
  }

  | SearchRemoteUser of {
    username: string;
    domain: string option;
  }

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


  let handle_local_post pool user scope post_to title content_type content =
    log.debug (fun f -> f "working on local post by %s of %s" (user.Database.LocalUser.username) content);

    let post_to = Option.get_or ~default:[] post_to
                |> List.filter (Fun.negate String.is_empty) in
    let+ _ =
      with_pool pool @@ 
      Ap_resolver.create_new_note scope user post_to [] title content content_type in

    Lwt.return_ok ()

  let handle_search_user pool username domain =
    log.debug (fun f ->
      f "received search query for user %s(@%a)?"
        username (Option.pp String.pp) domain);
    match domain with
    | None ->
      (* no domain given, we'll search any instances that don't have the user *)
      let+ instances =
        with_pool pool @@ fun db ->
        Database.RemoteInstance.find_possible_remote_instances_to_query
          ("%" ^ username ^ "%") db
        |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
      let+ query_res =
        Lwt_list.map_p (fun instance ->
          let domain = instance.Database.RemoteInstance.url in
          let+ _ = with_pool pool @@ fun db ->
            Ap_resolver.resolve_remote_user ~username ~domain db
            |> map_err (fun err -> `DatabaseError err) in
          return_ok ()
        ) instances
        |> lift_pure in
      List.iter (function Ok () -> () | Error err ->
        let _, msg, details = Error_handling.extract_error_details err in
        log.error (fun f -> f "search query failed with error %s: %s" msg details)
      ) query_res;
      return_ok ()      
    | Some domain ->
      log.debug (fun f ->  f "resolving user %s@%s?" username domain);
      let+ _ = with_pool pool @@ fun db ->
        Ap_resolver.resolve_remote_user ~username ~domain db
        |> map_err (fun err -> `DatabaseError err) in
      log.debug (fun f ->  f "successfully resolved user %s@%s" username domain);
      return_ok ()

  let handle_follow_remote_user pool user username domain =
    log.debug (fun f ->
      f "handling follow remote user %s@%s by %s" username domain (user.Database.LocalUser.username)
    );
    let+ _ =
      with_pool pool @@ fun db ->
      Ap_resolver.follow_remote_user user ~username ~domain db
      |> map_err (fun err -> `ResolverError err) in
    return_ok ()

  let handle_accept_follow pool follow_id =
    log.debug (fun f -> f "worker accepting follow %s" follow_id);
    let+ follow = 
      with_pool pool @@ fun db ->
      Database.Follows.lookup_by_url ~url:follow_id db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    log.debug (fun f -> f "worker found follow with id %s" follow_id);
    let+ follow = Lwt.return (match follow with None -> Error (`WorkerError "no follow found") | Some follow -> Ok follow) in
    let+ _ =
      with_pool pool @@ fun db ->
      Database.Follows.update_pending_status ~id:follow.Database.Follows.id ~pending:false db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    log.debug (fun f -> f "worker updated follow status");
    return_ok ()

  let handle_remote_follow pool id actor target raw =
    with_pool pool @@ fun db ->
    Ap_resolver.follow_local_user id actor target raw db
    |> map_err (fun err -> `DatabaseError err)

  let handle_undo_follow pool follow_id =
    let+ follow =
      with_pool pool @@ fun db ->
      Database.Follows.lookup_by_url ~url:follow_id db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    let+ follow = Lwt.return (match follow with None -> Error (`WorkerError "no follow found") | Some follow -> Ok follow) in
    let+ () =
      with_pool pool @@ fun db ->    
      Database.Follows.delete ~id:(follow.Database.Follows.id) db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    return_ok ()

  let handle_create_remote_note pool author direct_message (note: Activitypub.Types.note) =

    log.debug (fun f -> f "creating remote note!");
    let url = note.id in
    let raw_data = note.raw in
    let post_source = Option.get_or ~default:note.content note.source in
    let published =
      Option.get_lazy (Ptime_clock.now) (note.published) in
    let summary = Option.filter (Fun.negate String.is_empty) note.summary in

    let is_public, is_follower_public = ref false, ref false in
    let extract_target =
      let lazy local_user_regex =
        Configuration.Regex.local_user_id_format in
      fun to_ ->
      if String.equal to_ Activitypub.Constants.ActivityStreams.public
      then (is_public := true; return_ok None)
      else match Re.exec_opt local_user_regex to_ with
        | Some group ->
          let username = Re.Group.get group 1 in
          let+ local_user =
            with_pool pool @@ fun db ->
            Database.LocalUser.find_user ~username db
            |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
          begin match local_user with
          | None -> return_ok None
          | Some local_user ->
            let+ user =
              with_pool pool @@ fun db ->
              Database.Actor.create_local_user ~local_id:(local_user.Database.LocalUser.id) db
              |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
            return_ok (Some user)
          end
        | None ->
          is_follower_public := !is_follower_public ||
                                Uri.of_string to_ |> Uri.path |> String.split_on_char '/'
                                |> List.last_opt |> Option.exists (String.equal "followers");
          return_ok None in
    let+ to_ =
      Lwt_list.map_p extract_target note.to_
      >> List.filter_map (function
        | Ok v -> v
        | Error err ->
          let _, msg, details = Error_handling.extract_error_details err in
          log.debug (fun f -> f "resolving target of post errored with msg %s: %s" msg details);
          None
      ) |> lift_pure in
    let+ cc_ =
      Lwt_list.map_p extract_target note.cc
      >> List.filter_map (function
        | Ok v -> v
        | Error err ->
          let _, msg, details = Error_handling.extract_error_details err in
          log.debug (fun f -> f "resolving ccd of post errored with msg %s: %s" msg details);
          None
      ) |> lift_pure in
    let+ author =
      with_pool pool @@ fun db ->
      Database.Actor.create_remote_user ~remote_id:(author.Database.RemoteUser.id) db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in

    let+ post =
      with_pool pool @@ fun db -> 
      Database.Posts.create
        ?summary
        ~raw_data
        ~url
        ~author
        ~is_public:(!is_public && not direct_message)
        ~is_follower_public:(!is_follower_public)
        ~post_source
        ~post_content:`Text
        ~published db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    log.debug (fun f -> f "created post");

    let+ _ =
      with_pool pool @@ fun db ->
      Database.Posts.add_post_tos ~id:(post.Database.Posts.id) ~tos:to_ db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    log.debug (fun f -> f "to_s added");
    let+ _ =
      with_pool pool @@ fun db ->
      Database.Posts.add_post_ccs ~id:(post.Database.Posts.id) ~ccs:cc_ db
      |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
    log.debug (fun f -> f "ccs added");

    return_ok ()

  let worker (pool: (Caqti_lwt.connection, [> Caqti_error.t]) Caqti_lwt.Pool.t) task_in =
    log.debug (fun f -> f "worker now waiting for task");
    Lwt_stream.iter_s 
      (fun task ->
         log.debug (fun f -> f "worker woken up with task");
         try
           begin match[@warning "-8"] task with
           | HandleUndoFollow {follow_id} ->
             let res = handle_undo_follow pool follow_id in
             handle_error res
           | HandleRemoteFollow {id; actor; target; raw} ->
             let res = handle_remote_follow pool id actor target raw in
             handle_error res
           | HandleAcceptFollow {follow_id} ->
             let res = handle_accept_follow pool follow_id in
             handle_error res
           | SearchRemoteUser {username; domain} ->
             let res = handle_search_user pool username domain in
             handle_error res
           | LocalPost {user; title; content; content_type; scope; post_to;} -> 
             let res = handle_local_post pool user scope post_to title content_type content in
             handle_error res
           | FollowRemoteUser {user; username; domain} ->
             let res = handle_follow_remote_user pool user username domain in
             handle_error res             
           | CreateRemoteNote { author; direct_message; note } ->
             let res = handle_create_remote_note pool author direct_message note in
             handle_error res             

           end |> Lwt.map ignore
         with
         | exn ->
           log.debug (fun f -> f "worker failed with exn %s" (Printexc.to_string exn));
           Lwt.return ()
      )
      task_in

end

open (struct
  let send_task_internal = ref None
end)

let send_task : task -> unit =
  let task_fun = lazy (Option.get_exn_or "send task without initialising worker" !send_task_internal) in
  fun task ->
    (Lazy.force task_fun) (Some task)

let init () =
  let task_in, send_task = Lwt_stream.create () in
  send_task_internal := Some send_task;
  (* hackity hack hacks to get access to Dream's internal Sql pool *)
  let pool =
    let vl = ref None in
    ignore @@ Dream.sql_pool (Configuration.(Lazy.force database_uri)) (fun req ->
      vl :=(Dream__sql.Sql.Message.field req Dream__sql.Sql.pool_field);
      Dream.html ""
    ) (Dream.request "");
    match !vl with
    | None -> raise (Failure "worker failed to acquire access to pool")
    | Some pool -> pool in
  (* configure journal with WAL and busy timeout to avoid busy errors *)
  log.info (fun f -> f "setting up worker database configurations");
  Lwt.join [
    worker (Obj.magic pool) task_in;
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

