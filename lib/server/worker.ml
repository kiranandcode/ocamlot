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
        log.warning (fun f -> f "worker error: %s" msg);
        log.debug (fun f -> f "worker error details: %s" details);
        Lwt.return ()
      | Error err ->
        let _, msg, details = Error_handling.extract_error_details err in
        log.warning (fun f -> f "worker error: %s" msg);
        log.debug (fun f -> f "worker error details: %s" details);
        Lwt.return ()
    )

  let resolve_remote_user pool username domain =
    with_pool pool @@ fun db ->
    let+ remote_user = Resolver.resolve_remote_user ~username ~domain db in

    return_ok ()
    


  let handle_local_post pool config user scope post_to title content_type content =
    log.debug (fun f -> f "working on local post by %s of %s" (Database.LocalUser.username user) content);
    let id = Database.Activity.fresh_id () in
    let+ post =
      with_pool pool @@ fun db ->
      (* lookup the author *)
      let+ author = (Database.Actor.of_local (Database.LocalUser.self user) db)
                    |> map_err (fun err -> `DatabaseError err) in
      log.debug (fun f -> f "retreived user %s" (Database.LocalUser.username user));

      let is_public, is_follower_public =
        match scope with
        | `DM -> false, false
        | `Followers -> false, true
        | `Public -> true, true in

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
    let+ _ =
      let user_tag = Configuration.Regex.user_tag config |> Re.compile in
      Lwt_list.map_p (fun tagged_user ->
        let matches = Re.all user_tag (String.trim tagged_user) in
        let+ matches = return (match matches with h :: _ -> Ok h
                                                | _ -> Error (`Msg "Invalid string")) in
        let _username = Re.Group.get matches 0 in
        let _domain = Re.Group.get matches 1 in
        Lwt.return_ok ()
      ) (Option.value ~default:[] post_to) |> lift_pure in

      
    (* collect targets *)


    log.debug (fun f -> f "completed user's %s post" (Database.LocalUser.username user));
    Lwt.return_ok ()

  let worker (pool: (Caqti_lwt.connection, [> Caqti_error.t]) Caqti_lwt.Pool.t) task_in config =
    log.debug (fun f -> f "worker now waiting for task");
    Lwt_stream.iter_s 
      (fun task ->
         log.debug (fun f -> f "worker woken up with task");
         begin match task with
         | LocalPost {user; title; content; content_type; scope; post_to;} -> 
           let res = handle_local_post pool config user scope post_to title content_type content in
           handle_error res
         end |> Lwt.map ignore )
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

