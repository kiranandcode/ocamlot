[@@@warning "-33-27"]
open Containers
open Common

let log = Logging.add_logger "worker"

let lift_database_error res =
  map_err (fun err -> `DatabaseError (Caqti_error.show err)) res
let lift_resolver_error res =
  map_err (fun err -> `ResolverError err) res

let filter_map_list ~msg f ls =
  Lwt_list.map_s f ls
  |> Lwt.map (List.filter_map (function
      | Ok v -> v
      | Error err ->
        let _, header, details = Error_handling.extract_error_details err in
        log.debug (fun f -> f "%s failed with %s: %s" msg header details);
        None
    ))
  |> lift_pure

let iter_list ~msg f ls =
  Lwt_list.map_s f ls
  |> Lwt.map (List.iter (function
      | Ok _ -> ()
      | Error err ->
        let _, header, details = Error_handling.extract_error_details err in
        log.debug (fun f -> f "%s failed with %s: %s" msg header details);
        ()
    ))
  |> lift_pure


type task =
  | CreateRemoteNote of {
    author : string;
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

  | HandleRemoteLike of {
      id: string;
      published: Ptime.t;
      target: Database.Posts.t;
      author: string;
      raw_data: Yojson.Safe.t;
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

  let uri_ends_with_followers to_ =
    Uri.of_string to_ |> Uri.path |> String.split_on_char '/'
    |> List.last_opt |> Option.exists (String.equal "followers")

  let extract_local_target_link pool to_ =
    let lazy local_user_regex =
      Configuration.Regex.local_user_id_format in
    if String.equal to_ Activitypub.Constants.ActivityStreams.public
    then return_ok None
    else match Re.exec_opt local_user_regex to_ with
      | Some group ->
        let username = Re.Group.get group 1 in
        let+ local_user =
          with_pool pool @@ fun db ->
          Database.LocalUser.find_user ~username db
          |> lift_database_error in
        begin match local_user with
          | None -> return_ok None
          | Some local_user ->
            let+ user =
              with_pool pool @@ fun db ->
              Database.Actor.create_local_user ~local_id:(local_user.Database.LocalUser.id) db
              |> lift_database_error in
            return_ok (Some user)
        end
      | None ->
        return_ok None

  let handle_error res =
    Lwt.bind res (function
        | Ok _ -> Lwt.return ()
        | Error (#Caqti_error.t as err) ->
          let _, msg, details =
            Error_handling.extract_error_details
              (`DatabaseError (Caqti_error.show err)) in
          log.warning (fun f -> f "worker error: %s" msg);
          log.debug (fun f -> f "worker error details: %s" details);
          Lwt.return ()
        | Error err ->
          let _, msg, details = Error_handling.extract_error_details err in
          log.warning (fun f -> f "worker error: %s" msg);
          log.debug (fun f -> f "worker error details: %s" details);
          Lwt.return ()
      )

  let handle_local_post pool user scope post_to title content_type content =
    log.debug (fun f ->
        f "working on local post by %s of %s"
          (user.Database.LocalUser.username) content);

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
      let+ _ = with_pool pool @@ fun db ->
        Ap_resolver.resolve_remote_user ~username ~domain db
        |> lift_resolver_error in
      log.debug (fun f ->  f "successfully resolved user %s@%s" username domain);
      return_ok ()

  let handle_follow_remote_user pool user username domain =
    log.debug (fun f ->
        f "handling follow remote user %s@%s by %s"
          username domain (user.Database.LocalUser.username));
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
      |> lift_database_error in
    log.debug (fun f -> f "worker found follow with id %s" follow_id);
    let+ follow = get_opt follow
        ~else_:(fun () -> (`WorkerError "no follow found"))  in
    let+ _ =
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
    let+ author =
      with_pool pool @@ fun db ->
      Ap_resolver.resolve_remote_user_by_url (Uri.of_string author) db
      |> lift_resolver_error in
    let+ follow = 
      with_pool pool @@ fun db ->
      Database.Likes.create
        ~raw_data ~url:id
        ~post:target.Database.Posts.id ~published
        ~actor:author.Database.RemoteUser.id db
      |> lift_database_error in
    log.debug (fun f -> f "worker added remote like");
    return_ok ()

  let handle_undo_follow pool follow_id =
    let+ follow =
      with_pool pool @@ fun db ->
      Database.Follows.lookup_by_url ~url:follow_id db
      |> lift_database_error in
    let+ follow = get_opt follow
        ~else_:(fun () -> (`WorkerError "no follow found")) in
    let+ () =
      with_pool pool @@ fun db ->    
      Database.Follows.delete ~id:(follow.Database.Follows.id) db
      |> lift_database_error in
    return_ok ()

  let handle_create_remote_note pool author direct_message (note: Activitypub.Types.note) =
    let+ author =
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
  let+ to_ =
    filter_map_list ~msg:"resolving target of post" (extract_local_target_link pool) note.to_ in
  let+ cc_ =
    filter_map_list ~msg:"resolving ccd of post" (extract_local_target_link pool) note.cc in
  let+ author =
    with_pool pool @@ fun db ->
    Database.Actor.create_remote_user ~remote_id:(author.Database.RemoteUser.id) db
    |> lift_database_error in
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
    |> lift_database_error in
  log.debug (fun f -> f "created post");

  let+ _ =
    with_pool pool @@ fun db ->
    Database.Posts.add_post_tos ~id:(post.Database.Posts.id) ~tos:to_ db
    |> lift_database_error  in
  log.debug (fun f -> f "to_s added");
  let+ _ =
    with_pool pool @@ fun db ->
    Database.Posts.add_post_ccs ~id:(post.Database.Posts.id) ~ccs:cc_ db
    |> lift_database_error in
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
               handle_undo_follow pool follow_id
             | HandleRemoteFollow {id; actor; target; raw} ->
               handle_remote_follow pool id actor target raw
             | HandleAcceptFollow {follow_id} ->
               handle_accept_follow pool follow_id
             | HandleRemoteLike { id; published; target; author; raw_data } -> 
               handle_remote_like pool id published target author raw_data
             | SearchRemoteUser {username; domain} ->
               handle_search_user pool username domain
             | LocalPost {user; title; content; content_type; scope; post_to;} -> 
               handle_local_post pool user scope post_to title content_type content
             | FollowRemoteUser {user; username; domain} ->
               handle_follow_remote_user pool user username domain
             | CreateRemoteNote { author; direct_message; note } ->
               handle_create_remote_note pool author direct_message note
           end |> handle_error |> Lwt.map ignore
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

