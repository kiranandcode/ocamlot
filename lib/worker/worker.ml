[@@@warning "-33-27"]
open Containers
open Common
open Utils

module Task = Task


let handle_local_post pool user scope post_to title content_type content =
  log.debug (fun f ->
    f "working on local post by %s of %s"
      (user.Database.LocalUser.username) content);

  let post_to = Option.get_or ~default:[] post_to
                |> List.filter (Fun.negate String.is_empty) in
  log.debug (fun f -> f "posted to %a" (List.pp String.pp) post_to);
  let* _ =
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
  let* author =
    with_pool pool @@ fun db ->
    Ap_resolver.resolve_remote_user_by_url (Uri.of_string author) db
    |>  lift_resolver_error in
  log.debug (fun f -> f "creating remote note!");
  let url = note.id in
  let raw_data = note.raw in
  let post_source = Option.get_or ~default:note.content note.source in
  let published =
    Option.get_lazy (Ptime_clock.now) (note.published) in
  let summary = Option.filter (Fun.negate String.is_empty) note.summary in

  let is_public, is_follower_public = ref false, ref false in
  List.iter (fun to_ ->
    if String.equal to_ Activitypub.Constants.ActivityStreams.public then
      is_public := true;
    if uri_ends_with_followers to_ then
      is_follower_public := true          
  ) (note.to_ @ note.cc);
  let* to_ =
    filter_map_list ~msg:"resolving target of post" (extract_local_target_link pool) note.to_ in
  let* cc_ =
    filter_map_list ~msg:"resolving ccd of post" (extract_local_target_link pool) note.cc in
  let* author =
    with_pool pool @@ fun db ->
    Database.Actor.create_remote_user ~remote_id:(author.Database.RemoteUser.id) db
    |> lift_database_error in
  let* post =
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
    |> lift_database_error in
  log.debug (fun f -> f "created post");

  let* _ =
    with_pool pool @@ fun db ->
    Database.Posts.add_post_tos ~id:(post.Database.Posts.id) ~tos:to_ db
    |> lift_database_error  in
  log.debug (fun f -> f "to_s added");
  let* _ =
    with_pool pool @@ fun db ->
    Database.Posts.add_post_ccs ~id:(post.Database.Posts.id) ~ccs:cc_ db
    |> lift_database_error in
  log.debug (fun f -> f "ccs added");
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
         | LocalPost {user; title; content; content_type; scope; post_to;} -> 
           handle_local_post pool user scope post_to title content_type content
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
       let* _ = DB.find enable_journal_mode () in
       let* _ = DB.find update_busy_timout () in
       Lwt.return_ok ()
     ) pool)
     |> Obj.magic
     |> handle_error)
  ]

