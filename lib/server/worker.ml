open Containers
open Common

let sanitize res = res |> Lwt_result.map_error (fun e -> `Msg e)
(* let (let+!) x f = Lwt_result.bind (sanitize x) f *)

type task =
  | CreateNote of {
    id: string;
    author: string;
    to_: string list; cc: string list;
    sensitive: bool; direct_message: bool;
    content: string; source: string option; summary: string option;
    published: Ptime.t option;
    tags: [ `Raw of Yojson.Safe.t | `Value of Activitypub.Types.tag ] list;
    data: Yojson.Safe.t;
  }
  (* follow by a local user *)
  | LocalFollow of {
    local: Database.LocalUser.t;   (* local user doing the following  *)
    username: string;              (* remote username being followed  *)
    domain: string;                (* remote domain being followed *)
  }
  | RecordAcceptLocalFollow of {
      follow: Database.Follow.t;     (* follow being accepted *)
      author: Database.LocalUser.t;  (* local user who made the request accepted *)
      target: Database.RemoteUser.t; (* url of remote user doing the accepting *)
    }

  (* follow by remote user *)
  | RemoteFollow of {
    id: string;                    (* url of the follow object *)
    remote: string;                (* url of remote actor making the follow *)
    target: Database.LocalUser.t;  (* url of local actor being followed *)
    data: Yojson.Safe.t;           (* raw data of the follow object *)
  }
  | AcceptRemoteFollow of {
      follow: Database.Follow.t;
      author: Database.RemoteUser.t;
      target: Database.LocalUser.t;
    }

  (* post by local user *)
  | LocalPost of {
      user: Database.LocalUser.t;
      content: string;
    }

open struct

  let worker_var = Lwt_mvar.create_empty ()

  let rec worker (pool: (Caqti_lwt.connection, [> Caqti_error.t]) Caqti_lwt.Pool.t) config =
    let+ task = Lwt_mvar.take worker_var in
    match task with
    | CreateNote { id; author; to_; cc;
                   sensitive; direct_message;
                   source; summary; content;
                   published; tags; data; } ->
      let res =
        Fun.flip Caqti_lwt.Pool.use pool @@ fun db ->
        let+ author =
          let+ author = Resolver.resolve_remote_user_by_url (Uri.of_string author) db
                         |> sanitize in
          Database.Actor.of_remote (Database.RemoteUser.self author) db  |> sanitize
        in
        let+ is_public, to_ =
          Lwt_list.fold_left_s (fun acc vl ->
            let+ (is_public, tgts) = return acc in
            match vl with
            | "https://www.w3.org/ns/activitystreams#Public" -> Lwt.return_ok (true, tgts)
            | target ->
              let+ target =
                let user_re = Re.compile (Configuration.Regex.local_user_id_format config) in
                match Re.exec_opt user_re target with
                | None ->
                  let+ remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string target) db in
                  Database.Actor.of_remote (Database.RemoteUser.self remote_user) db
                | Some matches ->
                  let username = Re.Group.get matches 1 in
                  let+ local_user = Database.LocalUser.lookup_user_exn ~username db in
                  Database.Actor.of_local (Database.LocalUser.self local_user) db in
              Lwt.return_ok (is_public, target :: tgts)
          ) (Ok (not sensitive && not direct_message, [])) to_
          |> sanitize in
        let+ cc =
          Lwt_list.fold_left_s (fun tgts target ->
            let+ tgts = Lwt.return tgts in
            if String.suffix ~suf:"followers" target
            then Lwt.return_ok tgts
            else let+ target =
                let user_re = Re.compile (Configuration.Regex.local_user_id_format config) in
                match Re.exec_opt user_re target with
                | None ->
                  let+ remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string target) db in
                  Database.Actor.of_remote (Database.RemoteUser.self remote_user) db
                | Some matches ->
                  let username = Re.Group.get matches 1 in
                  let+ local_user = Database.LocalUser.lookup_user_exn ~username db in
                  Database.Actor.of_local (Database.LocalUser.self local_user) db in
              Lwt.return_ok (target :: tgts)
          ) (Ok []) cc
          |> sanitize in

        let+ mentions, tags =
          Lwt_list.fold_left_s (fun acc vl ->
            let+ (mentions, tags) = return acc in
            match vl with
            | `Value Activitypub.Types.{ ty=`Hashtag; href; name } ->
              let+ tag = 
                let+ tag = Database.Tag.create name db in
                Lwt.return_ok (Database.Tag.self tag, Some href) in
              Lwt.return_ok (mentions,tag :: tags)
            | `Value Activitypub.Types.{ ty=`Mention; href; name=_ } ->
              let+ target =
                let user_re = Re.compile (Configuration.Regex.local_user_id_format config) in
                match Re.exec_opt user_re href with
                | None ->
                  let+ remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string href) db in
                  Database.Actor.of_remote (Database.RemoteUser.self remote_user) db
                | Some matches ->
                  let username = Re.Group.get matches 1 in
                  let+ local_user = Database.LocalUser.lookup_user_exn ~username db in
                  Database.Actor.of_local (Database.LocalUser.self local_user) db in
              Lwt.return_ok (target :: mentions,tags)
            |  _ -> Lwt.return_ok (mentions,tags)
          ) (Ok ([],[])) tags
          |> sanitize in
        let published = Option.value ~default:(Ptime_clock.now ()) published
                        |> Ptime.to_float_s |> CalendarLib.Calendar.from_unixfloat in
        let post_source = Option.value ~default:content source in
        let+ post = 
          Database.Post.create_post
            ?summary ~url:id  ~published ~author ~is_public ~post_source
            ~raw_data:(Yojson.Safe.to_string data) db |> sanitize in
        let+ () = Database.Post.add_post_tos (Database.Post.self post) to_ db |> sanitize in
        let+ () = Database.Post.add_post_ccs (Database.Post.self post) cc db |> sanitize in
        let+ () = Database.Post.add_post_tags (Database.Post.self post) tags db |> sanitize in
        let+ () = Database.Post.add_post_mentions (Database.Post.self post) mentions db |> sanitize in

        Lwt_result.return ()
      in
      Lwt.bind res @@ fun res ->
      begin match res with
      | Ok () -> worker pool config
      | Error e ->
        let e = match e with
          | #Caqti_error.t as e -> Caqti_error.show e
          | `Msg e -> e in
        Dream.error (fun log -> log "error in worker: %s" e);
        worker pool config
      end        
    | AcceptRemoteFollow {follow; author; target} ->
      let res =
        Fun.flip Caqti_lwt.Pool.use pool @@ fun db ->
        Resolver.accept_remote_follow config follow author target db |> sanitize in
      Lwt.bind res @@ fun res ->
      begin match res with
      | Ok () -> worker pool config
      | Error e ->
        let e = match e with
          | #Caqti_error.t as e -> Caqti_error.show e
          | `Msg e -> e in
        Dream.error (fun log -> log "error in worker: %s" e);
        worker pool config
      end
    | RecordAcceptLocalFollow {follow; author; target} ->
      let res =
        Fun.flip Caqti_lwt.Pool.use pool @@ fun db ->
        Resolver.accept_local_follow config follow ~author ~target db |> sanitize in
      Lwt.bind res @@ fun res ->
      begin match res with
      | Ok () -> worker pool config
      | Error e ->
        let e = match e with
          | #Caqti_error.t as e -> Caqti_error.show e
          | `Msg e -> e in
        Dream.error (fun log -> log "error in worker: %s" e);
        worker pool config
      end
    | RemoteFollow { id; remote; target; data } ->
      let res =
        Fun.flip Caqti_lwt.Pool.use pool @@ fun db ->
        Resolver.follow_local_user config id remote target data db |> sanitize in
      Lwt.bind res @@ fun res ->
      begin match res with
      | Ok () -> worker pool config
      | Error e ->
        let e = match e with
          | #Caqti_error.t as e -> Caqti_error.show e
          | `Msg e -> e in
        Dream.error (fun log -> log "error in worker: %s" e);
        worker pool config
      end
    | LocalFollow {local; username; domain} ->
      let res =
        Fun.flip Caqti_lwt.Pool.use pool @@ fun db ->
        Resolver.follow_remote_user config local ~username ~domain db |> sanitize in
      Lwt.bind res @@ fun res -> 
      begin match res with
      | Ok () -> worker pool config
      | Error e ->
        let e = match e with
          | #Caqti_error.t as e -> Caqti_error.show e
          | `Msg e -> e in
        Dream.error (fun log -> log "error in worker: %s" e);
        worker pool config
      end
    | LocalPost {user; content} ->
      let id = Database.Activity.fresh_id () in
      let post = 
        Fun.flip Caqti_lwt.Pool.use pool @@ fun db ->
        let+ author = (Database.Actor.of_local
                          (Database.LocalUser.self user) db) |> sanitize in
        Database.Post.create_post
          ~public_id:(Database.Activity.id_to_string id)
          ~url:(Configuration.Url.activity_endpoint config
                  (Database.Activity.id_to_string id)
                |> Uri.to_string)
          ~author ~is_public:true ~post_source:content
          ~published:(CalendarLib.Calendar.now ()) db |> sanitize in
      Lwt.bind post @@ fun post ->
      match post with
      | Ok _ -> worker pool config
      | Error e ->
        let e = match e with
          | #Caqti_error.t as e -> Caqti_error.show e
          | `Msg e -> e in
        Dream.error (fun log -> log "error in worker: %s" e);
        worker pool config

end

let send _req task  =
  Lwt.async (fun () -> Lwt_mvar.put worker_var task)

let init config =
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
  let+ () =
    let enable_journal_mode =
      Caqti_request.Infix.(Caqti_type.unit -->. Caqti_type.unit @:-  "PRAGMA journal_mode=WAL" ) in
    let update_busy_timout =
      Caqti_request.Infix.(Caqti_type.unit -->. Caqti_type.unit @:-  "PRAGMA busy_timeout = 5000" ) in
    Caqti_lwt.Pool.use (fun (module DB : Caqti_lwt.CONNECTION) ->
      let+ () = DB.exec enable_journal_mode () in
      DB.exec update_busy_timout ()
    ) pool in
  worker (pool :> (Caqti_lwt.connection, [Caqti_error.t | `Msg of string] ) Caqti_lwt.Pool.t) config
