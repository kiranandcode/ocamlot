[@@@warning "-26-27-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:RemoteInstance *)
type t = Types.remote_instance

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

let find_possible_remote_instances_to_query =
  let%sql.query find_remote_instances_request = {|
SELECT RI.id, RI.url, RI.last_unreachable
FROM RemoteInstance as RI
WHERE 
   NOT EXISTS(
     SELECT RU.id, RU.username, RU.instance_id, RU.display_name, RU.url, RU.inbox, RU.outbox, RU.followers, RU.following, RU.summary, RU.public_key_pem 
     FROM RemoteUser as RU
     WHERE RU.instance_id = RI.id AND (RU.username LIKE ? OR RU.display_name LIKE ?)
   )
ORDER BY RI.url DESC
|} in
  let%sql.query find_remote_instances_offset_request = {|
SELECT RI.id, RI.url, RI.last_unreachable
FROM RemoteInstance as RI
WHERE 
   NOT EXISTS(
     SELECT RU.id, RU.username, RU.instance_id, RU.display_name, RU.url, RU.inbox, RU.outbox, RU.followers, RU.following, RU.summary, RU.public_key_pem 
     FROM RemoteUser as RU
     WHERE RU.instance_id = RI.id AND (RU.username LIKE ? OR RU.display_name LIKE ?)
   )
ORDER BY RI.url DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset query (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list find_remote_instances_request (query, query)
      |> flatten_error
    | Some (limit, offset) ->
      DB.collect_list find_remote_instances_offset_request (query, query, limit, offset)
      |> flatten_error


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
