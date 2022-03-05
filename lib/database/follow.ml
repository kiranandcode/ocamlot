[@@@warning "-33"]
open Containers
open Utils

(* see ./resources/schema.sql:Post *)
type t = {
  id: int64;                              (* unique internal id of the follow *)
  public_id: string option;               (* public id of the follow object if made locally  *)
  url: string;                            (* url of the follow  *)
  raw_data: string option;                (* raw json of the follow if external  *)
  mutable pending: bool option;                   (* whether the request is pending *)

  author: int64;                          (* author of the follow (may be local, or remote) *)
  target: int64;                          (* target of the follow (may be local, or remote) *)
}

let t =
  let encode { id; public_id; url; raw_data; pending; author; target } =
    Ok (id, public_id, url, (raw_data, pending, author, target)) in
  let decode (id, public_id, url, (raw_data, pending, author, target)) =
    Ok { id; public_id; url; raw_data; pending; author; target } in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 (option string) string (tup4 (option string) (option bool) int64 int64))

let create_follow =
  Caqti_request.exec ~oneshot:false T.Std.(tup4 (option string) string (option string) (tup3 (option bool) int64 int64)) {|
INSERT OR IGNORE INTO Follows (public_id, url, raw_data, pending, author_id, target_id) VALUES (?, ?, ?, ?, ?, ?)
|}

let lookup_follow_by_public_id_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, raw_data, pending, author_id, target_id FROM Posts WHERE public_id = ?
|}

let lookup_follow_by_url_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, raw_data, pending, author_id, target_id FROM Posts WHERE url = ?
|}

let update_follow_pending_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup2 (option bool) int64) {| UPDATE Follows SET pending = ? WHERE id = ?  |}

let resolve_follow_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, raw_data, author_id, target_id FROM Posts WHERE id = ?
|}

let resolve_follow id (module DB: DB) =
  DB.find resolve_follow_request id |> flatten_error

let create_follow ?public_id ?raw_data ?pending ~url
      ~author:((author_id, _) : Actor.t Link.t)
      ~target:((target_id, _) : Actor.t Link.t) (module DB: DB) =
  let* () = DB.exec create_follow (public_id, url, raw_data, (pending, author_id, target_id)) |> flatten_error in
  DB.find lookup_follow_by_url_request url |> flatten_error

let lookup_follow_by_url url (module DB: DB) =
  DB.find_opt lookup_follow_by_url_request url |> flatten_error
let lookup_follow_by_url_exn url (module DB: DB) =
  DB.find lookup_follow_by_url_request url |> flatten_error

let lookup_follow_by_public_id public_id (module DB: DB) =
  DB.find_opt lookup_follow_by_public_id_request public_id |> flatten_error
let lookup_post_by_public_id_exn public_id (module DB: DB) =
  DB.find lookup_follow_by_public_id_request public_id |> flatten_error

let update_follow_pending_status follow status (module DB: DB) =
  let* () = DB.exec update_follow_pending_request (status, follow.id) |> flatten_error in
  follow.pending <- status;
  R.return ()

let self t : t Link.t = t.id, resolve_follow
let public_id t = t.public_id
let author t : Actor.t Link.t = t.author, Actor.resolve
let target t : Actor.t Link.t = t.target, Actor.resolve
let pending t = t.pending
let url t = t.url
let raw_data t = t.raw_data
