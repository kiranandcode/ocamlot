(* open Containers *)
open Common

type task =
  | Follow of {
    local: Database.LocalUser.t;
    username: string;
    domain: string;
  }
  | Post of {
      user: Database.LocalUser.t;
      content: string;
    }

open struct

  let worker_var = Lwt_mvar.create_empty ()

  let rec worker config =
    let+ db, task = Lwt_mvar.take worker_var in
    match task with
    | Follow {local; username; domain} ->
      let+ res = Resolver.follow_remote_user config local ~username ~domain db in
      begin match res with
      | Ok () -> worker config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker config
      end
    | Post {user; content} ->
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
      | Ok _ -> worker config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker config


end

let send req task  =
  let add = Dream.sql req (fun db -> Lwt_mvar.put worker_var (db,task)) in
  Lwt.async (fun () -> add)

let init config =
  Lwt.async @@ fun () -> worker config
