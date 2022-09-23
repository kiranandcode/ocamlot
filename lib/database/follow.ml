[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:Follows *)
type t = Types.follow
let t = Types.follow


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

let resolve_follow =
  let%sql.query resolve_follow_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE id = ?
|} in
  fun  id (module DB: DB) ->
    DB.find resolve_follow_request id |> flatten_error

let create_follow  =
  let%sql.query create_follow =
    {|
INSERT OR IGNORE
INTO Follows (public_id, url, raw_data, pending, created, updated, author_id, target_id)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)
|} in
  fun ?public_id ?raw_data ?updated ~url 
    ~author:((author_id, _) : Actor.t Link.t)
    ~target:((target_id, _) : Actor.t Link.t)
    ~pending ~created
    (module DB: DB) ->
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

let update_follow_pending_status =
  let%sql.query update_follow_pending_request =
    {| UPDATE Follows SET pending = ?, updated = ? WHERE id = ?  |} in
  fun  ?timestamp ((follow_id, _): t Link.t) status (module DB: DB) ->
    let timestamp = Option.value ~default:(CalendarLib.Calendar.now ()) timestamp in
    let* () = DB.exec update_follow_pending_request (status, Some timestamp, follow_id)
              |> flatten_error in
    R.return ()

let delete_follow =  
  let%sql.query delete_follow_request = {|
DELETE FROM Follows
WHERE id = ?
LIMIT 1
|} in
  fun ((follow_id,_): t Link.t) (module DB: DB) ->
  let* () = DB.exec delete_follow_request follow_id |> flatten_error in
  R.return ()

let collect_follows_for_actor =
  let%sql.query collect_related_follows_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|} in
  let%sql.query collect_related_follows_offset_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE (target_id = ? OR author_id = ?) AND DATETIME(COALESCE(updated, created)) <= ? AND pending = TRUE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|} in
  fun  ?offset ((actor_id, _): Actor.t Link.t) (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list collect_related_follows_request (actor_id, actor_id)
      |> flatten_error
    | Some (timestamp, limit, offset) ->
      DB.collect_list collect_related_follows_offset_request
        (actor_id, actor_id, timestamp, (limit, offset))
      |> flatten_error

let is_following =
  let%sql.query is_following_request = {|
SELECT COUNT(*)
FROM Follows
WHERE author_id = ? AND target_id = ? AND pending = FALSE
|} in
  fun ~author:((author_id, _): Actor.t Link.t) ~target:((target_id, _): Actor.t Link.t) (module DB: DB) ->
    DB.find is_following_request (author_id, target_id)
    |> flatten_error
    |> Lwt_result.map (fun c -> c > 0)
    
let find_follow_between =
  let%sql.query find_follow_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND target_id = ? AND pending = FALSE
|} in
  fun ~author:((author_id, _): Actor.t Link.t) ~target:((target_id, _): Actor.t Link.t) (module DB: DB) ->
    DB.find_opt find_follow_request (author_id, target_id)
    |> flatten_error

let find_follow_between_exn =
  let%sql.query find_follow_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND target_id = ? AND pending = FALSE
|} in
  fun ~author:((author_id, _): Actor.t Link.t) ~target:((target_id, _): Actor.t Link.t) (module DB: DB) ->
    DB.find find_follow_request (author_id, target_id)
    |> flatten_error


let count_following =
  let%sql.query count_following_request = {|
SELECT COUNT(*)
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|} in
  fun  ((actor_id, _): Actor.t Link.t) (module DB: DB) ->
    DB.find count_following_request actor_id
    |> flatten_error

let collect_following =
  let%sql.query collect_following_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|} in
  let%sql.query collect_following_offset_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE author_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset ((actor_id, _): Actor.t Link.t) (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list collect_following_request (actor_id)
      |> flatten_error
    | Some (timestamp, limit, offset) ->
      DB.collect_list collect_following_offset_request
        (actor_id, timestamp, limit, offset)
      |> flatten_error

let count_followers =
  let%sql.query count_followers_request = {|
SELECT COUNT(*)
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|} in
  fun  ((actor_id, _): Actor.t Link.t) (module DB: DB) ->
    DB.find count_followers_request actor_id
    |> flatten_error

let collect_followers =
  let%sql.query collect_followers_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
|} in
  let%sql.query collect_followers_offset_request = {|
SELECT id, public_id, url, raw_data, pending, created, updated, author_id, target_id
FROM Follows
WHERE target_id = ? AND DATETIME(COALESCE(updated, created)) <= ? AND pending = FALSE
ORDER BY DATETIME(COALESCE(updated, created)) DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset ((actor_id, _): Actor.t Link.t) (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list collect_followers_request actor_id
      |> flatten_error
    | Some (timestamp, limit, offset) ->
      DB.collect_list collect_followers_offset_request
        (actor_id, timestamp, limit, offset)
      |> flatten_error


let self (t: t) : t Link.t = t.id, resolve_follow
let public_id (t: t) = t.public_id
let author (t: t) : Actor.t Link.t = t.author, Actor.resolve
let target (t: t) : Actor.t Link.t = t.target, Actor.resolve
let pending (t: t) = t.pending
let url (t: t) = t.url
let raw_data (t: t) = t.raw_data
let created (t: t) = t.created
let updated (t: t) = t.updated
