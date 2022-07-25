[@@@warning "-32-33"]
open Utils
let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:Like *)
type%sql.generate t = SQL [@schema "Likes"]

let%sql.query resolve_like_by_id_request = {|
SELECT id, public_id, url, raw_data, published, post_id, actor_id FROM Likes WHERE id = ?
|}


let%sql.query create_like_request = {|
INSERT OR IGNORE
INTO Likes (public_id, url, raw_data, published, post_id, actor_id)
VALUES (?, ?, ?, ?, ?, ?)
|}

let%sql.query collect_likes_by_post_id_request = {|
SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE post_id = ?
ORDER BY datetime(published)
|}

let%sql.query collect_likes_by_actor_id_request = {|
SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE actor_id = ?
ORDER BY datetime(published)
|}

let%sql.query collect_likes_by_actor_id_offset_request = {|
SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE actor_id = ? AND published <= ?
ORDER BY datetime(published) DESC
LIMIT ? OFFSET ?
|}


let lookup_like_by_url_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE url = ?
|}

let lookup_like_by_public_id_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, public_id, url, raw_data, published, post_id, actor_id
FROM Likes
WHERE public_id = ?
|}


let resolve id (module DB: DB) =
  DB.find resolve_like_by_id_request id |> flatten_error

let create ?public_id ?raw_data ~url
      ~post:((post_id, _) : Post.t Link.t)
      ~actor:((actor_id, _) : Actor.t Link.t)
      ~published (module DB: DB) =
  let* () = DB.exec create_like_request
              (public_id, url, raw_data, (published, post_id, actor_id))
            |> flatten_error in
  DB.find lookup_like_by_url_request url |> flatten_error

let lookup_like_by_url url (module DB: DB) =
  DB.find_opt lookup_like_by_url_request url |> flatten_error
let lookup_like_by_url_exn url (module DB: DB) =
  DB.find lookup_like_by_url_request url |> flatten_error

let lookup_like_by_public_id public_id (module DB: DB) =
  DB.find_opt lookup_like_by_public_id_request public_id |> flatten_error
let lookup_like_by_public_id_exn public_id (module DB: DB) =
  DB.find lookup_like_by_public_id_request public_id |> flatten_error

let collect_likes_for_post ((post_id, _): Post.t Link.t) (module DB: DB) =
  DB.collect_list collect_likes_by_post_id_request post_id |> flatten_error

let collect_likes_for_actor ?offset ((post_id, _): Actor.t Link.t) (module DB: DB) =
  match offset with
  | None -> 
    DB.collect_list collect_likes_by_actor_id_request post_id |> flatten_error
  | Some (date, count, offset) ->
    DB.collect_list collect_likes_by_actor_id_offset_request
      (post_id, date, count, offset)
    |> flatten_error    

let self t : t Link.t = t.id, resolve
let public_id t = t.public_id
let url t = t.url
let raw_data t = t.raw_data
let post t : Post.t Link.t = t.post_id, Post.resolve_post
let target t : Actor.t Link.t = t.actor_id, Actor.resolve
let published t : Calendar.t = t.published
