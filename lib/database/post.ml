[@@@warning "-33"]
open Containers
open Utils

(* see ./resources/schema.sql:Post *)
type t = {
  id: int64;                              (* unique internal id of the post *)
  public_id: string option;               (* public id of the post if made locally  *)
  url: string;                            (* url of the post  *)
  author: int64;                          (* author of the post (may be local, or remote) *)
  raw_text: string option;                (* raw json of the post if external  *)
}

let t =
  let encode {id;public_id;url;author;raw_text} =
    Ok (id, public_id, url, (author, raw_text)) in
  let decode (id, public_id, url, (author, raw_text)) =
    Ok {id;public_id;url;author;raw_text} in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 (option string) string (tup2 int64 (option string)))

let create_post_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup4 (option string) string int64 (option string)) {|
INSERT OR IGNORE INTO Posts (public_id, url, author_id, raw_data) VALUES (?, ?, ?, ?)
|}

let lookup_post_by_public_id_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, author_id, raw_data FROM Posts WHERE public_id = ?
|}

let lookup_post_by_url_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, author_id, raw_data FROM Posts WHERE url = ?
|}

let collect_posts_by_author_request =
  Caqti_request.collect ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, author_id, raw_data FROM Posts WHERE author_id = ?
|}

let resolve_post_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, author_id, raw_data FROM Posts WHERE id = ?
|}

let resolve_post id (module DB: DB) =
  DB.find resolve_post_request id |> flatten_error

let create_post ?public_id ?raw_data ~url ~author:((author_id, _) : Actor.t Link.t) (module DB: DB) =
  let* () = DB.exec create_post_request (public_id, url, author_id, raw_data) |> flatten_error in
  DB.find lookup_post_by_url_request url |> flatten_error

let lookup_post_by_url url (module DB: DB) =
  DB.find_opt lookup_post_by_url_request url |> flatten_error
let lookup_post_by_url_exn url (module DB: DB) =
  DB.find lookup_post_by_url_request url |> flatten_error


let lookup_post_by_public_id public_id (module DB: DB) =
  DB.find_opt lookup_post_by_public_id_request public_id |> flatten_error
let lookup_post_by_public_id_exn public_id (module DB: DB) =
  DB.find lookup_post_by_public_id_request public_id |> flatten_error

let collect_posts_by_author ((author_id, _) : Actor.t Link.t) (module DB: DB) =
  DB.collect_list collect_posts_by_author_request author_id |> flatten_error

let self t : t Link.t = t.id, resolve_post
let public_id t = t.public_id
let author t : Actor.t Link.t = t.author, Actor.resolve
let url t = t.url
let raw_data t = t.raw_text
