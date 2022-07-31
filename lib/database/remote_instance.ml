[@@@warning "-26-27-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:RemoteInstance *)
type t = Types.remote_instance

let t =
  let encode ({id;url;last_unreachable}: t) =
    Ok (id, url, last_unreachable) in
  let decode (id, url, last_unreachable) =
    Ok ({id;url;last_unreachable}: t) in
  T.Std.custom ~encode ~decode
    T.Std.(tup3 int64 string (option timestamp))

let%sql.query lookup_instance_request = {|
SELECT id, url, last_unreachable FROM RemoteInstance WHERE url = ?
 |}

let%sql.query update_instance_request = {|
UPDATE RemoteInstance SET last_unreachable = ? WHERE id = ?
|}

let resolve_instance =
  let%sql.query resolve_instance_request = {|
SELECT id, url, last_unreachable FROM RemoteInstance WHERE id = ?
|} in
 fun id (module DB: DB) ->
  DB.find resolve_instance_request id
  |> flatten_error

let lookup_instance hostname (module DB: DB) =
  DB.find_opt lookup_instance_request hostname |> flatten_error

let lookup_instance_exn hostname (module DB: DB) =
  DB.find lookup_instance_request hostname |> flatten_error

let create_instance =
  let%sql.query create_instance_request = {|
INSERT OR IGNORE INTO RemoteInstance (url, last_unreachable)  VALUES (?, ?)
|} in
  fun hostname (module DB: DB) ->
    let* () = DB.exec create_instance_request (hostname,None) |> flatten_error in
    DB.find lookup_instance_request hostname |> flatten_error

let record_instance_reachable (remote_instance: t) (module DB: DB) =
  let* () = DB.exec update_instance_request (None, remote_instance.id) |> flatten_error in
  remote_instance.last_unreachable <- None;
  R.return ()

let record_instance_unreachable (remote_instance: t) (module DB: DB) =
  let time = Calendar.now () in
  let* () = DB.exec update_instance_request (Some time, remote_instance.id) |> flatten_error in
  remote_instance.last_unreachable <- Some time;
  R.return ()

let self (t: t) : t Link.t = t.id, resolve_instance
let url (t: t) = t.url
let last_unreachable (t: t) = t.last_unreachable
