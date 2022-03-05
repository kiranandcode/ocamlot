[@@@warning "-33"]
open Containers
open Utils

(* see ./resources/schema.sql:RemoteInstance *)
type t = {
  id: int64;                              (* unique internal id of remote user *)
  username: string;                       (* username of the user *)
  instance_id: int64;                     (* id of remote instance  *)
  display_name: string option;            (* display name of user *)
  url: string;                            (* url to the user - equiv to their id *)
}

let t =
  let encode {id;username;instance_id;display_name;url} =
    Ok (id, username, instance_id, (display_name, url)) in
  let decode (id, username, instance_id, (display_name, url)) =
    Ok {id;username; instance_id;display_name;url} in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 string int64 (tup2 (option string) string))

let create_remote_user_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup4 string int64 (option string) string) {|
INSERT INTO RemoteUser (username, instance_id, display_name, url)  VALUES (?, ?, ?, ?)
|}

let lookup_remote_user_by_address_request =
  Caqti_request.find ~oneshot:false T.Std.(tup2 string string) t {|
SELECT RemoteUser.id, RemoteUser.username, RemoteUser.instance_id, RemoteUser.display_name, RemoteUser.url
FROM RemoteUser JOIN RemoteInstance on RemoteUser.instance_id = RemoteInstance.id 
WHERE RemoteInstance.url = ? AND RemoteUser.username = ?
|}

let lookup_remote_user_by_url_request =
  Caqti_request.find ~oneshot:false T.Std.string t
    {| SELECT id, username, instance_id, display_name, url FROM RemoteUser WHERE url = ?  |}

let resolve_remote_user_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t
    {| SELECT id, username, instance_id, display_name, url FROM RemoteUser WHERE id = ?  |}

let retrieve_known_user_list_reqest =
  Caqti_request.collect ~oneshot:false T.Std.unit T.Std.(tup3 string string string) {|
SELECT RemoteUser.username, RemoteInstance.url, RemoteUser.url FROM RemoteUser 
JOIN RemoteInstance on RemoteUser.instance_id = RemoteInstance.id
|}

let resolve_remote_user id (module DB: DB) =
  DB.find resolve_remote_user_request id |> flatten_error

let create_remote_user ?display_name
      ~username ~instance:((instance, _): Remote_instance.t Link.t) ~url
      (module DB: DB) =
  let* () = DB.exec create_remote_user_request (username, instance, display_name, url)
            |> flatten_error in
  flatten_error @@ DB.find lookup_remote_user_by_url_request url
  
let lookup_remote_user_by_url url (module DB: DB) =
  flatten_error @@ DB.find_opt lookup_remote_user_by_url_request url
let lookup_remote_user_by_url_exn url (module DB: DB) =
  flatten_error @@ DB.find lookup_remote_user_by_url_request url


let lookup_remote_user_by_address ~username ~domain (module DB: DB) =
  flatten_error @@ DB.find_opt lookup_remote_user_by_address_request (domain, username)
let lookup_remote_user_by_address_exn ~username ~domain (module DB: DB) =
  flatten_error @@ DB.find lookup_remote_user_by_address_request (domain, username)

let get_known_remote_actors (module DB: DB) =
  DB.collect_list retrieve_known_user_list_reqest () |> flatten_error


let self t : t Link.t = t.id, resolve_remote_user
let username t = t.username
let instance t : Remote_instance.t Link.t = t.instance_id, Remote_instance.resolve_instance
let display_name t = t.display_name |> Option.value ~default:t.username
let url t = t.url
