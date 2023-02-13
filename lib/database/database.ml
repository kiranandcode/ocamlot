module Tables = Tables

module Interface = Interface

include Operations

let resolve_actor ~id db =
  let open Lwt_result.Syntax in
  let* l = Actor.resolve ~id db in
  match l with 
  | `Local l ->
    let+ l = LocalUser.resolve ~id:l db in
    `Local l.LocalUser.username
  | `Remote r ->
    let+ r = RemoteUser.resolve ~id:r db in
    `Remote r.RemoteUser.url  

let resolve_actor_url ~id db =
  Lwt_result.map (function
      | `Local username ->
        Configuration.Url.user username
        |> Uri.to_string
      | `Remote url -> url)
    (resolve_actor ~id db)

