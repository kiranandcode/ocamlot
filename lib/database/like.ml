open Utils


(* see ./resources/schema.sql:Like *)
type t = {
  id: int64;                              (* internal id of the like *)

  public_id: string option;               (* public id of the mention object if made externally *)
  url: string;                            (* url of the mention  *)
  raw_data: string option;                (* raw json of the mention if external  *)

  post_id: int64;                         (* post being liked *)
  actor_id: int64;                        (* actor doing the liking *)
}

let t =
  let encode {id; public_id; url; raw_data; post_id; actor_id} =
    Ok (id, public_id, url, (raw_data, post_id, actor_id)) in
  let decode (id, public_id, url, (raw_data, post_id, actor_id)) =
    Ok {id; public_id; url; raw_data; post_id; actor_id} in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 (option string) string (tup3 (option string) int64 int64))

let resolve_like_by_id_request =
  Caqti_request.find ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, raw_data, post_id, actor_id FROM Likes WHERE id = ?
|}


let create_like_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup4 (option string) string (option string) (tup2 int64 int64)) {|
INSERT OR IGNORE INTO Likes (public_id, url, raw_data, post_id, actor_id) VALUES (?, ?, ?, ?, ?)
|}

let collect_likes_by_post_id_request =
  Caqti_request.collect ~oneshot:false T.Std.int64 t {|
SELECT id, public_id, url, raw_data, post_id, actor_id FROM Likes WHERE post_id = ?
|}

let lookup_like_by_url_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, raw_data, post_id, actor_id FROM Likes WHERE url = ?
|}

let lookup_like_by_public_id_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT id, public_id, url, raw_data, post_id, actor_id FROM Likes WHERE public_id = ?
|}


let resolve id (module DB: DB) =
  DB.find resolve_like_by_id_request id |> flatten_error

let create ?public_id ?raw_data ~url
      ~post:((post_id, _) : Post.t Link.t)
      ~actor:((actor_id, _) : Actor.t Link.t) (module DB: DB) =
  let* () = DB.exec create_like_request (public_id, url, raw_data, (post_id, actor_id))  |> flatten_error in
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

let self t : t Link.t = t.id, resolve
let public_id t = t.public_id
let url t = t.url
let raw_data t = t.raw_data
let post t : Post.t Link.t = t.post_id, Post.resolve_post
let target t : Actor.t Link.t = t.actor_id, Actor.resolve

