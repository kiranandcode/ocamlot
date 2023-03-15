[@@@warning "-33-27"]
open Containers
open Common
open Utils

module Task = Task


let handle_local_post pool user scope post_to title content_type content in_reply_to attachments =
  log.debug (fun f ->
    f "working on local post by %s of %s"
      (user.Database.LocalUser.username) content);

  let post_to = Option.get_or ~default:[] post_to
                |> List.filter (Fun.negate String.is_empty) in
  log.debug (fun f -> f "posted to [%a]" (List.pp String.pp) post_to);
  let* _ =
    with_pool pool @@ 
    Ap_resolver.create_new_note scope user post_to [] title content content_type in_reply_to attachments in

  Lwt.return_ok ()

let handle_search_user pool username domain =
  log.debug (fun f ->
    f "received search query for user %s(@%a)?"
      username (Option.pp String.pp) domain);
  match domain with
  | None ->
    (* no domain given, we'll search any instances that don't have the user *)
    let* instances =
      with_pool pool @@ fun db ->
      Database.RemoteInstance.find_possible_remote_instances_to_query
        ("%" ^ username ^ "%") db
      |> lift_database_error in
    (* for each remote instance *)
    iter_list ~msg:"search query" (fun instance ->
      let domain = instance.Database.RemoteInstance.url in
      (* attempt to resolve the remote user *)
      with_pool pool @@ fun db ->
      Ap_resolver.resolve_remote_user ~username ~domain db
      |> lift_resolver_error
    ) instances
  | Some domain ->
    log.debug (fun f ->  f "resolving user %s@%s?" username domain);
    let* _ = with_pool pool @@ fun db ->
      Ap_resolver.resolve_remote_user ~username ~domain db
      |> lift_resolver_error in
    log.debug (fun f ->  f "successfully resolved user %s@%s" username domain);
    return_ok ()

let handle_follow_remote_user pool user username domain =
  log.debug (fun f ->
    f "handling follow remote user %s@%s by %s"
      username domain (user.Database.LocalUser.username));
  let* _ =
    with_pool pool @@ fun db ->
    Ap_resolver.follow_remote_user user ~username ~domain db
    |> map_err (fun err -> `ResolverError err) in
  return_ok ()

let handle_unfollow_remote_user pool user username domain =
  log.debug (fun f ->
    f "handling unfollow remote user %s@%s by %s"
      username domain (user.Database.LocalUser.username));
  let* _ =
    with_pool pool @@ fun db ->
    Ap_resolver.unfollow_remote_user user ~username ~domain db
    |> map_err (fun err -> `ResolverError err) in
  return_ok ()


let handle_accept_follow pool follow_id =
  log.debug (fun f -> f "worker accepting follow %s" follow_id);
  let* follow = 
    with_pool pool @@ fun db ->
    Database.Follows.lookup_by_url ~url:follow_id db
    |> lift_database_error in
  log.debug (fun f -> f "worker found follow with id %s" follow_id);
  let* follow = get_opt follow
                  ~else_:(fun () -> (`WorkerError "no follow found"))  in
  let* _ =
    with_pool pool @@ fun db ->
    Database.Follows.update_pending_status ~id:follow.Database.Follows.id ~pending:false db
    |> lift_database_error in
  log.debug (fun f -> f "worker updated follow status");
  return_ok ()

let handle_remote_follow pool id actor target raw =
  with_pool pool @@ fun db ->
  Ap_resolver.follow_local_user id actor target raw db
  |> lift_resolver_error

let handle_remote_like pool id published target author raw_data =
  log.debug (fun f -> f "worker handling remote like");
  let* author =
    with_pool pool @@ fun db ->
    Ap_resolver.resolve_remote_user_by_url (Uri.of_string author) db
    |> lift_resolver_error in
  let* author =
    with_pool pool (Database.Actor.create_remote_user
                      ~remote_id:author.Database.RemoteUser.id) in
  let* follow = 
    with_pool pool @@ fun db ->
    Database.Likes.create
      ~raw_data ~url:id
      ~post:target.Database.Posts.id ~published
      ~actor:author db
    |> lift_database_error in
  log.debug (fun f -> f "worker added remote like");
  return_ok ()

let handle_remote_reboost pool id published target author raw_data =
  log.debug (fun f -> f "worker handling remote reboost by %s" author);
  let* author =
    with_pool pool @@ fun db ->
    Ap_resolver.resolve_remote_user_by_url (Uri.of_string author) db
    |> lift_resolver_error in
  let* author =
    with_pool pool (Database.Actor.create_remote_user
                      ~remote_id:author.Database.RemoteUser.id) in
  let* reboost = 
    with_pool pool @@ fun db ->
    Database.Reboosts.create
      ~raw_data ~url:id
      ~post:target.Database.Posts.id ~published
      ~actor:author db
    |> lift_database_error in
  log.debug (fun f -> f "worker added remote reboost");
  return_ok ()

let handle_local_like pool (author: Database.LocalUser.t) (target: Database.Posts.t) =
  let* like =
    let* author = with_pool pool (Database.Actor.create_local_user ~local_id:author.id) in
    with_pool pool @@ Database.Likes.find_like_between
                        ~post:target.id ~author in
  match like with
  | Some _ -> Lwt.return_ok ()
  | None ->
    log.debug (fun f -> f "working on local like by %s of %s" (author.Database.LocalUser.username) target.url);
    let* _ = with_pool pool @@ Ap_resolver.create_new_like author target in
    log.debug (fun f -> f "successfully sent local like by %s" author.Database.LocalUser.username);
    Lwt.return_ok ()

let handle_local_reboost pool (author: Database.LocalUser.t) (target: Database.Posts.t) =
  let* reboost =
    let* author = with_pool pool (Database.Actor.create_local_user ~local_id:author.id) in
    with_pool pool @@ Database.Reboosts.find_reboost_between ~post:target.id ~author in
  match reboost with
  | Some _ -> Lwt.return_ok ()
  | None ->
    log.debug (fun f -> f "working on local reboost by %s of %s"
                          (author.Database.LocalUser.username) target.url);
    let* _ = with_pool pool @@ Ap_resolver.create_new_reboost author target in
    log.debug (fun f -> f "successfully sent local reboost by %s"
                          author.Database.LocalUser.username);
    Lwt.return_ok ()

let handle_undo_follow pool follow_id =
  let* follow =
    with_pool pool @@ fun db ->
    Database.Follows.lookup_by_url ~url:follow_id db
    |> lift_database_error in
  let* follow = get_opt follow
                  ~else_:(fun () -> (`WorkerError "no follow found")) in
  let* () =
    with_pool pool @@ fun db ->    
    Database.Follows.delete ~id:(follow.Database.Follows.id) db
    |> lift_database_error in
  return_ok ()

let handle_create_remote_note pool author direct_message (note: Activitypub.Types.note) =
  let* _ = with_pool pool @@ fun db -> Ap_resolver.insert_remote_note ~direct_message ~author note db in
  log.debug (fun f -> f "created post");
  return_ok ()

let handle_undo pool author obj =
  with_pool pool (Ap_resolver.undo_object author obj)

let worker (pool: (Caqti_lwt.connection, [> Caqti_error.t]) Caqti_lwt.Pool.t) task_in =
  log.debug (fun f -> f "worker now waiting for task");
  Lwt_stream.iter_s
    (fun task ->
       log.debug (fun f -> f "worker woken up with task");
       try
         begin match[@warning "-8"] (task: Task.t) with
         | HandleUndoFollow {follow_id} ->
           handle_undo_follow pool follow_id
         | HandleRemoteFollow {id; actor; target; raw} ->
           handle_remote_follow pool id actor target raw
         | HandleAcceptFollow {follow_id} ->
           handle_accept_follow pool follow_id
         | HandleRemoteLike { id; published; target; author; raw_data } -> 
           handle_remote_like pool id published target author raw_data
         | HandleRemoteReboost { id; published; target; author; raw_data } -> 
           handle_remote_reboost pool id published target author raw_data
         | SearchRemoteUser {username; domain} ->
           handle_search_user pool username domain
         | LocalPost {user; title; content; content_type; scope; post_to; in_reply_to; attachments} -> 
           handle_local_post pool user scope post_to title content_type content in_reply_to attachments
         | FollowRemoteUser {user; username; domain} ->
           handle_follow_remote_user pool user username domain
         | CreateRemoteNote { author; direct_message; note } ->
           handle_create_remote_note pool author direct_message note
         | LocalLike {user; post} ->
           handle_local_like pool user post
         | LocalReboost {user; post} ->
           handle_local_reboost pool user post
         | HandleUndo {author; obj} ->
           handle_undo pool author obj
         | UnfollowRemoteUser { user; username; domain } ->
           handle_unfollow_remote_user pool user username domain
         end |> handle_error |> Lwt.map ignore
       with
       | exn ->
         log.debug (fun f -> f "worker failed with exn %s" (Printexc.to_string exn));
         Lwt.return ()
    )
    task_in


let send_task_internal = ref None
let send_task : Task.t -> unit =
  let task_fun = lazy (Option.get_exn_or "send task without initialising worker" !send_task_internal) in
  fun task ->
    (Lazy.force task_fun) (Some task)

let post_connect (module DB : Caqti_lwt.CONNECTION) =
  let enable_journal_mode =
    Caqti_request.Infix.(Caqti_type.unit -->! Caqti_type.string @:-  "PRAGMA journal_mode=WAL" ) in
  let update_busy_timout =
    Caqti_request.Infix.(Caqti_type.unit -->! Caqti_type.int @:-  "PRAGMA busy_timeout = 50000" ) in
  ((let* _ = DB.find enable_journal_mode () in
    let* _ = DB.find update_busy_timout () in
    Lwt.return_ok ()))

let init () =
  let task_in, send_task = Lwt_stream.create () in
  send_task_internal := Some send_task;
  (* hackity hack hacks to get access to Dream's internal Sql pool *)
  let db_uri = Lazy.force Configuration.database_uri in
  let pool = Caqti_lwt.connect_pool ~post_connect (Uri.of_string db_uri)
             |> Result.get_exn in
  (* configure journal with WAL and busy timeout to avoid busy errors *)
  log.info (fun f -> f "setting up worker database configurations");
  worker (Obj.magic pool) task_in
