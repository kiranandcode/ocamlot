[@@@warning "-33"]
open Containers
open Utils

(* see ./resources/schema.sql:Follows *)
type t = {
  id: int64;                              (* unique internal id of the follow *)
  public_id: string option;               (* public id of the follow object if made locally  *)
  url: string;                            (* url of the follow  *)
  raw_data: string option;                (* raw json of the follow if external  *)
  pending: bool;                          (* whether the request is pending *)

  created: CalendarLib.Calendar.t;        (* date on which follow was created *)
  updated: CalendarLib.Calendar.t option; (* date on which follow was updated *)

  author: int64;                          (* author of the follow (may be local, or remote) *)
  target: int64;                          (* target of the follow (may be local, or remote) *)
}

let t =
  let encode { id; public_id; url; raw_data; pending; created; updated; author; target } =
    Ok (id, public_id, url,
        (raw_data, pending, created,
         (updated, author, target))) in
  let decode (id, public_id, url,
              (raw_data, pending, created,
               (updated, author, target))) =
    Ok { id; public_id; url; raw_data; pending; created; updated; author; target } in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 (option string) string
             (tup4 (option string) bool
                timestamp
                (tup3 (option timestamp) int64 int64)))

let create_follow =
  Caqti_request.exec ~oneshot:false
    T.Std.(tup4 (option string) string (option string)
             (tup4 bool timestamp (option timestamp)
                (tup2 int64 int64)))
    {|
INSERT OR IGNORE
INTO Follows (public_id, url, raw_data, pending, created, updated, author_id, target_id)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)
|}

let lookup_follow_by_public_id_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE public_id = ?
|}

let lookup_follow_by_url_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE url = ?
|}

let update_follow_pending_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup3 bool timestamp int64)
    {| UPDATE Follows SET pending = ?, updated = ? WHERE id = ?  |}

let resolve_follow_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE id = ?
|}

let collect_related_follows_request =
  Caqti_request.find ~oneshot:false T.Std.(tup2 int64 int64) t {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_related_follows_offset_request =
  Caqti_request.find ~oneshot:false
    T.Std.(tup4 int64 int64 timestamp (tup2 int int)) t
 {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND DATETIME(COALESCE(updated, created)) <= ? AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|}

let count_following_request =
  Caqti_request.find ~oneshot:false T.Std.int64 T.Std.int {|
SELECT COUNT(*)
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_following_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_following_offset_request =
  Caqti_request.find ~oneshot:false
    T.Std.(tup4 int64 timestamp int int) t
 {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|}

let count_followers_request =
  Caqti_request.find ~oneshot:false T.Std.int64 T.Std.int {|
SELECT COUNT(*)
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}


let collect_followers_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_followers_offset_request =
  Caqti_request.find ~oneshot:false
    T.Std.(tup4 int64 timestamp int int) t
 {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|}


let delete_follow_request =
  Caqti_request.exec ~oneshot:false T.Std.int64 {|
DELETE FROM Follows
WHERE id = ?
LIMIT 1
|}


let resolve_follow id (module DB: DB) =
  DB.find resolve_follow_request id |> flatten_error

let create_follow ?public_id ?raw_data ?updated ~url 
      ~author:((author_id, _) : Actor.t Link.t)
      ~target:((target_id, _) : Actor.t Link.t)
      ~pending ~created
      (module DB: DB) =
  let* () = DB.exec create_follow (public_id, url, raw_data,
                                   (pending, created, updated,
                                    (author_id, target_id))) |> flatten_error in
  DB.find lookup_follow_by_url_request url |> flatten_error

let lookup_follow_by_url url (module DB: DB) =
  DB.find_opt lookup_follow_by_url_request url |> flatten_error
let lookup_follow_by_url_exn url (module DB: DB) =
  DB.find lookup_follow_by_url_request url |> flatten_error

let lookup_follow_by_public_id public_id (module DB: DB) =
  DB.find_opt lookup_follow_by_public_id_request public_id |> flatten_error
let lookup_follow_by_public_id_exn public_id (module DB: DB) =
  DB.find lookup_follow_by_public_id_request public_id |> flatten_error

let update_follow_pending_status ?timestamp ((follow_id, _): t Link.t) status (module DB: DB) =
  let timestamp = Option.value ~default:(CalendarLib.Calendar.now ()) timestamp in
  let* () = DB.exec update_follow_pending_request (status, timestamp, follow_id)
            |> flatten_error in
  R.return ()

let delete_follow ((follow_id,_): t Link.t) (module DB: DB) =
  let* () = DB.exec delete_follow_request follow_id |> flatten_error in
  R.return ()

let collect_follows_for_actor ?offset ((actor_id, _): Actor.t Link.t) (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_related_follows_request (actor_id, actor_id)
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_related_follows_offset_request
      (actor_id, actor_id, timestamp, (limit, offset))
    |> flatten_error

let count_following ((actor_id, _): Actor.t Link.t) (module DB: DB) =
  DB.find count_following_request actor_id
  |> flatten_error

let collect_following ?offset ((actor_id, _): Actor.t Link.t) (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_following_request (actor_id)
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_following_offset_request
      (actor_id, timestamp, limit, offset)
    |> flatten_error

let count_followers ((actor_id, _): Actor.t Link.t) (module DB: DB) =
  DB.find count_followers_request actor_id
  |> flatten_error

let collect_followers ?offset ((actor_id, _): Actor.t Link.t) (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_followers_request actor_id
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_followers_offset_request
      (actor_id, timestamp, limit, offset)
    |> flatten_error


let self t : t Link.t = t.id, resolve_follow
let public_id t = t.public_id
let author t : Actor.t Link.t = t.author, Actor.resolve
let target t : Actor.t Link.t = t.target, Actor.resolve
let pending t = t.pending
let url t = t.url
let raw_data t = t.raw_data
let created t = t.created
let updated t = t.updated
