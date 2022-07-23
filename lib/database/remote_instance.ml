[@@@warning "-26-27-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:RemoteInstance *)
type%sql.check[@schema "RemoteInstance"] t = {
  id: int64;                              (* unique internal id of user *)
  url: string;                            (* url to instance *)
  mutable last_unreachable: timestamp option;    (* time since the sever was unreachable *)
}


let t =
  let encode {id;url;last_unreachable} =
    Ok (id, url, last_unreachable) in
  let decode (id, url, last_unreachable) =
    Ok {id;url;last_unreachable} in
  T.Std.custom ~encode ~decode
    T.Std.(tup3 int64 string (option timestamp))

let create_instance_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  tup2 string (option timestamp) -->. unit @:- {|
INSERT OR IGNORE INTO RemoteInstance (url, last_unreachable)  VALUES (?, ?)
|}

let resolve_instance_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->! t @:- {|
SELECT id, url, last_unreachable FROM RemoteInstance WHERE id = ?
|}

let lookup_instance_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, url, last_unreachable FROM RemoteInstance WHERE url = ?
 |}

let update_instance_request =
    let open Caqti_type.Std in
    let open Caqti_request.Infix in
    tup2 (option timestamp) int64 -->. unit @:- {|
UPDATE RemoteInstance SET last_unreachable = ? WHERE id = ?
|}

let resolve_instance id (module DB: DB) =
  DB.find resolve_instance_request id
  |> flatten_error

let lookup_instance hostname (module DB: DB) =
  DB.find_opt lookup_instance_request hostname |> flatten_error

let lookup_instance_exn hostname (module DB: DB) =
  DB.find lookup_instance_request hostname |> flatten_error

let create_instance hostname (module DB: DB) =
  let* () = DB.exec create_instance_request (hostname,None) |> flatten_error in
  DB.find lookup_instance_request hostname |> flatten_error

let record_instance_reachable remote_instance (module DB: DB) =
  let* () = DB.exec update_instance_request (None, remote_instance.id) |> flatten_error in
  remote_instance.last_unreachable <- None;
  R.return ()

let record_instance_unreachable remote_instance (module DB: DB) =
  let time = Calendar.now () in
  let* () = DB.exec update_instance_request (Some time, remote_instance.id) |> flatten_error in
  remote_instance.last_unreachable <- Some time;
  R.return ()

let self t : t Link.t = t.id, resolve_instance
let url t = t.url
let last_unreachable t = t.last_unreachable
