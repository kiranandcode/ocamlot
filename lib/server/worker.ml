open Containers
open Common

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

  let rec worker db config =
    let+ task = Lwt_mvar.take worker_var in
    let+ () = Lwt_unix.sleep 2.0 in
    match task with
    | CreateNote { id; author; to_; cc;
                   sensitive; direct_message;
                   source; summary; content;
                   published; tags; data; } ->
      let+ res =
        let+! author =
          let+! author = Resolver.resolve_remote_user_by_url (Uri.of_string author) db in
          Database.Actor.of_remote (Database.RemoteUser.self author) db  in
        let+ is_public, to_ =
          Lwt_list.fold_left_s (fun (is_public, tgts) -> function
            | "https://www.w3.org/ns/activitystreams#Public" -> Lwt.return (true, tgts)
            | target ->
              let+ target =
                let user_re = Re.compile (Configuration.Regex.local_user_id_format config) in
                match Re.exec_opt user_re target with
                | None ->
                  let+! remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string target) db in
                  Database.Actor.of_remote (Database.RemoteUser.self remote_user) db 
                | Some matches ->
                  let username = Re.Group.get matches 1 in
                  let+! local_user = Database.LocalUser.lookup_user_exn ~username db in
                  Database.Actor.of_local (Database.LocalUser.self local_user) db in
              Lwt.return (is_public, target :: tgts)
          ) (not sensitive && not direct_message, []) to_ in
        let to_ = List.filter_map Result.to_opt to_ in
        let+ cc =
          Lwt_list.fold_left_s (fun tgts target ->
            if String.suffix ~suf:"followers" target
            then Lwt.return tgts
            else let+ target =
                let user_re = Re.compile (Configuration.Regex.local_user_id_format config) in
                match Re.exec_opt user_re target with
                | None ->
                  let+! remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string target) db in
                  Database.Actor.of_remote (Database.RemoteUser.self remote_user) db 
                | Some matches ->
                  let username = Re.Group.get matches 1 in
                  let+! local_user = Database.LocalUser.lookup_user_exn ~username db in
                  Database.Actor.of_local (Database.LocalUser.self local_user) db in
              Lwt.return (target :: tgts)
          ) [] cc in
        let cc = List.filter_map Result.to_opt cc in

        let+ mentions, tags =
          Lwt_list.fold_left_s (fun (mentions, tags) -> function
            | `Value Activitypub.Types.{ ty=`Hashtag; href; name } ->
              let+ tag = 
                let+! tag = Database.Tag.create name db in
                Lwt.return_ok (Database.Tag.self tag, Some href) in
              Lwt.return (mentions,tag :: tags)
            | `Value Activitypub.Types.{ ty=`Mention; href; name=_ } ->
              let+ target =
                let user_re = Re.compile (Configuration.Regex.local_user_id_format config) in
                match Re.exec_opt user_re href with
                | None ->
                  let+! remote_user = Resolver.resolve_remote_user_by_url (Uri.of_string href) db in
                  Database.Actor.of_remote (Database.RemoteUser.self remote_user) db 
                | Some matches ->
                  let username = Re.Group.get matches 1 in
                  let+! local_user = Database.LocalUser.lookup_user_exn ~username db in
                  Database.Actor.of_local (Database.LocalUser.self local_user) db in
              Lwt.return (target :: mentions,tags)
            |  _ -> Lwt.return (mentions,tags)
          ) ([],[]) tags in
        let mentions = List.filter_map Result.to_opt mentions in
        let tags = List.filter_map Result.to_opt tags in
        let published = Option.value ~default:(Ptime_clock.now ()) published
                        |> Ptime.to_float_s |> CalendarLib.Calendar.from_unixfloat in
        let post_source = Option.value ~default:content source in
        let+! post = 
          Database.Post.create_post
            ?summary ~url:id  ~published ~author ~is_public ~post_source
            ~raw_data:(Yojson.Safe.to_string data) db in
        let+! () = Database.Post.add_post_tos (Database.Post.self post) to_ db in
        let+! () = Database.Post.add_post_ccs (Database.Post.self post) cc db in
        let+! () = Database.Post.add_post_tags (Database.Post.self post) tags db in
        let+! () = Database.Post.add_post_mentions (Database.Post.self post) mentions db in

        Lwt_result.return ()
      in
      begin match res with
      | Ok () -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config
      end        
    | AcceptRemoteFollow {follow; author; target} ->
      let+ res = Resolver.accept_remote_follow config follow author target db in
      begin match res with
      | Ok () -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config
      end
    | RecordAcceptLocalFollow {follow; author; target} ->
      let+ res = Resolver.accept_local_follow config follow ~author ~target db in
      begin match res with
      | Ok () -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config
      end
    | RemoteFollow { id; remote; target; data } ->
      let+ res = Resolver.follow_local_user config id remote target data db in
      begin match res with
      | Ok () -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config
      end
    | LocalFollow {local; username; domain} ->
      let+ res = Resolver.follow_remote_user config local ~username ~domain db in
      begin match res with
      | Ok () -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config
      end
    | LocalPost {user; content} ->
      let id = Database.Activity.fresh_id () in
      let+ post = 
        let+! author = (Database.Actor.of_local
                          (Database.LocalUser.self user) db) in
        Database.Post.create_post
          ~public_id:(Database.Activity.id_to_string id)
          ~url:(Configuration.Url.activity_endpoint config
                  (Database.Activity.id_to_string id)
                |> Uri.to_string)
          ~author ~is_public:true ~post_source:content
          ~published:(CalendarLib.Calendar.now ()) db in
      match post with
      | Ok _ -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config

end

let send _req task  =
  Lwt.async (fun () -> Lwt_mvar.put worker_var task)

let init config =
  Lwt.async @@ fun () ->
    let+ db =
      config
      |> Configuration.Params.database_path
      |> Uri.of_string
      |> Caqti_lwt.connect in
    let db = Result.get_exn db in
    worker db config
