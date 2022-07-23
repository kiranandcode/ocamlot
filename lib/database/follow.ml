[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:Follows *)
type%sql.generate t = SQL [@schema "Follows"]

let create_follow =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
 (tup4 (option string) string (option string)
             (tup4 bool timestamp (option timestamp)
                (tup2 int64 int64))) -->. unit @:-
    {|
INSERT OR IGNORE
INTO Follows (public_id, url, raw_data, pending, created, updated, author_id, target_id)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)
|}

let lookup_follow_by_public_id_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE public_id = ?
|}

let lookup_follow_by_url_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE url = ?
|}

let update_follow_pending_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  tup3 bool timestamp int64 -->. unit @:-
    {| UPDATE Follows SET pending = ?, updated = ? WHERE id = ?  |}

let resolve_follow_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->! t @:- {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE id = ?
|}

let collect_related_follows_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  (tup2 int64 int64) -->! t @:- {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_related_follows_offset_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  (tup4 int64 int64 timestamp (tup2 int int)) -->! t @:-
 {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND DATETIME(COALESCE(updated, created)) <= ? AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|}

let count_following_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->! T.Std.int @:- {|
SELECT COUNT(*)
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_following_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->* t @:- {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_following_offset_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  (tup4 int64 timestamp int int) -->* t @:-
 {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|}

let count_followers_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->! int @:- {|
SELECT COUNT(*)
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}


let collect_followers_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->* t @:- {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|}

let collect_followers_offset_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  (tup4 int64 timestamp int int) -->* t @:-
 {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|}


let delete_follow_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->. unit @:- {|
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
