open Containers
open Utils

(* see ./resources/schema.sql:Mention *)
type t = {
  public_id: string option;               (* public id of the follow object if made locally *)
  url: string;                            (* url of the mention  *)
  raw_data: string option;                (* raw json of the mention if external  *)

  post_id: int64;                         (* post doing the mentioning *)
  actor_id: int64;                        (* actor being mentioned by the post *)
}

let t =
  let encode {public_id; url; raw_data; post_id; actor_id} =
    Ok (public_id, url, raw_data, (post_id, actor_id)) in
  let decode (public_id, url, raw_data, (post_id, actor_id)) =
    Ok {public_id; url; raw_data; post_id; actor_id} in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 (option string) string (option string) (tup2 int64 int64))

let create_mention_request =
  Caqti_request.exec ~oneshot:false t {|
INSERT OR IGNORE INTO Mentions (public_id, url, raw_data, post_id, actor_id) VALUES (?, ?, ?, ?, ?)
|}

let collect_mentions_by_post_id_request =
  Caqti_request.collect ~oneshot:false T.Std.int64 t {|
SELECT public_id, url, raw_data, post_id, actor_id FROM Mentions WHERE post_id = ?
|}

let lookup_mentions_by_url_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT public_id, url, raw_data, post_id, actor_id FROM Mentions WHERE url = ?
|}

let lookup_mentions_by_public_id_request =
  Caqti_request.find ~oneshot:false T.Std.string t {|
SELECT public_id, url, raw_data, post_id, actor_id FROM Mentions WHERE public_id = ?
|}



let create_mention ?public_id ?raw_data ~url
      ~author:((post_id, _) : Post.t Link.t)
      ~target:((actor_id, _) : Actor.t Link.t) (module DB: DB) =
  let* () = DB.exec create_mention_request {public_id; url; raw_data; post_id; actor_id} |> flatten_error in
  DB.find lookup_mentions_by_url_request url |> flatten_error

let lookup_mention_by_url url (module DB: DB) =
  DB.find_opt lookup_mentions_by_url_request url |> flatten_error
let lookup_mention_by_url_exn url (module DB: DB) =
  DB.find lookup_mentions_by_url_request url |> flatten_error

let lookup_mention_by_public_id public_id (module DB: DB) =
  DB.find_opt lookup_mentions_by_public_id_request public_id |> flatten_error
let lookup_mention_by_url_exn public_id (module DB: DB) =
  DB.find lookup_mentions_by_public_id_request public_id |> flatten_error

let collect_mentions_for_post ((post_id, _): Post.t Link.t) (module DB: DB) =
  DB.collect_list collect_mentions_by_post_id_request post_id |> flatten_error

let public_id t = t.public_id
let author t : Actor.t Link.t = t.author, Actor.resolve
let target t : Actor.t Link.t = t.target, Actor.resolve
let pending t = t.pending
let url t = t.url
let raw_data t = t.raw_data
