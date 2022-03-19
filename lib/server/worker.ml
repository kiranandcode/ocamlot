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

  let rec worker db config =
    let+ task = Lwt_mvar.take worker_var in
    let+ () = Lwt_unix.sleep 2.0 in
    match task with
    | Follow {local; username; domain} ->
      let+ res = Resolver.follow_remote_user config local ~username ~domain db in
      begin match res with
      | Ok () -> worker db config
      | Error e ->
        Dream.error (fun log -> log "error in worker: %s" e);
        worker db config
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
    let db = Result.get_ok db in
    worker db config
