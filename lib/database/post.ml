[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:Post *)
type%sql.generate t = SQL [@schema "Posts"]

let%sql.query create_post_request =  {|
INSERT OR IGNORE
INTO Posts (public_id, url, author_id, is_public, summary, post_source, published, raw_data)
VALUES (?, ?, ?, ?, ?, ?, ?, ?)
|}

let lookup_post_by_public_id_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE public_id = ?
|}

let lookup_post_by_url_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE url = ?
|}


let%sql.query count_posts_by_author_request = {|
SELECT COUNT(*)
FROM Posts
WHERE author_id = ?
|}

let%sql.query collect_posts_by_author_request = {|
SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE author_id = ? AND is_public = TRUE
ORDER BY DATETIME(published) DESC
|}

let%sql.query collect_posts_by_author_offset_request = {|
SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE author_id = ? AND DATETIME(published) <= ? AND is_public = TRUE
ORDER BY DATETIME(published) DESC
LIMIT ? OFFSET ?
|}


let%sql.query resolve_post_request = {|
SELECT id, public_id, url, author_id, is_public, summary, post_source, published, raw_data
FROM Posts
WHERE id = ?
|}

let%sql.query insert_post_to_request = {|
INSERT OR IGNORE
INTO PostTo (post_id, actor_id)
VALUES (?, ?)
|}

let%sql.query collect_post_to_request =  {|
SELECT actor_id
From PostTo
WHERE post_id = ?
|}

let%sql.query insert_post_cc_request = {|
INSERT OR IGNORE
INTO PostCc (post_id, actor_id)
VALUES (?, ?)
|}

let%sql.query collect_post_cc_request = {|
SELECT actor_id
From PostCc
WHERE post_id = ?
|}

let%sql.query add_post_tag_request = {|
INSERT OR IGNORE
INTO PostTags (post_id, tag_id, url)
VALUES (?,?,?)
|}

let%sql.query collect_post_tags_request = {|
SELECT Tags.tag_id, Tags.tag_name, PostTags.url
FROM PostTags
JOIN Tags ON PostTags.tag_id = Tags.tag_id
WHERE PostTags.post_id = ?
|}

let%sql.query add_post_mention_request = {|
INSERT OR IGNORE
INTO Mention (post_id, actor_id)
VALUES (?,?)
|}

let%sql.query collect_post_mentions_request = {|
SELECT actor_id
FROM Mention
WHERE post_id = ?
|}

let%sql.query collect_post_feed_request = {|
-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND (
    -- where, we (1) are the author
    P.author_id = ? OR
-- or	we (1) are following the author of the post, and the post is public
    (EXISTS (SELECT * FROM Follows AS F WHERE F.author_id = ? AND F.target_id = P.author_id) AND P.is_public = TRUE) OR
-- or we (1) are the recipients (cc, to) of the post    
    (EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)))
ORDER BY DATETIME(P.published) DESC
|}


let%sql.query collect_post_feed_offset_request = {|
-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND 
    DATETIME(P.published) <= ? AND (
    -- where, we (1) are the author
    P.author_id = ? OR
-- or	we (1) are following the author of the post, and the post is public
    (EXISTS (SELECT * FROM Follows AS F WHERE F.author_id = ? AND F.target_id = P.author_id) AND P.is_public = TRUE) OR
-- or we (1) are the recipients (cc, to) of the post    
    (EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)))
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?
|}


let%sql.query collect_post_direct_messages_request = {|
-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND (
    -- where, we (1) are the author and the post is public
    (P.author_id = ? AND P.is_public = FALSE) OR
    -- or we (1) are the recipients (cc, to) of the post    
    ((EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)) AND
      P.is_public = FALSE))
ORDER BY DATETIME(P.published) DESC
|}

let%sql.query collect_post_direct_messages_offset_request = {|
-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
    DATETIME(P.published) <= ? AND (
    -- where, we (1) are the author and the post is public
    (P.author_id = ? AND P.is_public = FALSE) OR
    -- or we (1) are the recipients (cc, to) of the post    
    ((EXISTS (SELECT * FROM PostTo as PT WHERE PT.post_id = P.id AND PT.actor_id = ?) OR
EXISTS (SELECT * FROM PostCc as PC WHERE PC.post_id = P.id AND PC.actor_id = ?)) AND
      P.is_public = FALSE))
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?
|}


let%sql.query collect_post_whole_known_network_request = {|
-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND (P.is_public = TRUE)
ORDER BY DATETIME(P.published) DESC
|}

let%sql.query collect_post_whole_known_network_offset_request = {|
-- select posts 
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
    DATETIME(P.published) <= ? AND
    (P.is_public = TRUE)
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?
|}

let%sql.query collect_post_local_network_request = {|
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
    (P.is_public = TRUE) AND 
EXISTS (SELECT * FROM Actor as Act WHERE Act.id = P.author_id AND Act.local_id IS NOT NULL) 
ORDER BY DATETIME(P.published) DESC
|}


let%sql.query collect_post_local_network_offset_request = {|
SELECT P.id, P.public_id, P.url, P.author_id, P.is_public, P.summary, P.post_source, P.published, P.raw_data
FROM Posts as P
WHERE
    -- we are not blocking/muting the author 
    TRUE AND
   DATETIME(P.published) <= ? AND
    (P.is_public = TRUE) AND 
EXISTS (SELECT * FROM Actor as Act WHERE Act.id = P.author_id AND Act.local_id IS NOT NULL) 
ORDER BY DATETIME(P.published) DESC
LIMIT ? OFFSET ?
|}

let resolve_post id (module DB: DB) =
  DB.find resolve_post_request id |> flatten_error

let create_post ?public_id ?summary ?raw_data
      ~url ~author:((author_id, _) : Actor.t Link.t)
      ~is_public ~post_source ~published
      (module DB: DB) =
  let* () =
    DB.exec create_post_request
      (public_id, url, author_id,
       (is_public, summary, post_source,
        (published, raw_data))) |> flatten_error in
  DB.find lookup_post_by_url_request url |> flatten_error

let lookup_post_by_url url (module DB: DB) =
  DB.find_opt lookup_post_by_url_request url |> flatten_error
let lookup_post_by_url_exn url (module DB: DB) =
  DB.find lookup_post_by_url_request url |> flatten_error


let lookup_post_by_public_id public_id (module DB: DB) =
  DB.find_opt lookup_post_by_public_id_request public_id |> flatten_error
let lookup_post_by_public_id_exn public_id (module DB: DB) =
  DB.find lookup_post_by_public_id_request public_id |> flatten_error

let count_posts_by_author ((author_id, _) : Actor.t Link.t) (module DB: DB) =
  DB.find count_posts_by_author_request author_id |> flatten_error  

let collect_posts_by_author ?offset ((author_id, _) : Actor.t Link.t) (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_posts_by_author_request author_id |> flatten_error
  | Some (start, limit, offset) ->
    DB.collect_list collect_posts_by_author_offset_request (author_id, start, limit, offset)
    |> flatten_error    

let post_to ((id, _): t Link.t) (module DB: DB) : (Actor.t Link.t list, string) R.t =
  let* ls = DB.collect_list collect_post_to_request id
            |> flatten_error in
  let ls = List.map (fun id -> (id, Actor.resolve)) ls in
  Lwt_result.return ls

let post_cc ((id, _): t Link.t) (module DB: DB) : (Actor.t Link.t list, string) R.t =
  let* ls = DB.collect_list collect_post_cc_request id
            |> flatten_error in
  let ls = List.map (fun id -> (id, Actor.resolve)) ls in
  Lwt_result.return ls

let add_post_to ((p_id, _): t Link.t) ((a_id, _): Actor.t Link.t) (module DB: DB) =
  DB.exec insert_post_to_request (p_id, a_id)
  |> flatten_error

let add_post_tos (post: t Link.t) actors db =
  Lwt_list.fold_left_s (function
      Error _ as e -> fun _ -> Lwt.return e
    | Ok () -> fun a -> add_post_to post a db
  ) (Ok ()) actors

let add_post_cc ((p_id, _): t Link.t) ((a_id, _): Actor.t Link.t) (module DB: DB) =
  DB.exec insert_post_cc_request (p_id, a_id)
  |> flatten_error

let add_post_ccs (post: t Link.t) actors db =
  Lwt_list.fold_left_s (function
      Error _ as e -> fun _ -> Lwt.return e
    | Ok () -> fun a -> add_post_cc post a db
  ) (Ok ()) actors

let add_post_tag ?url (((post_id, _): t Link.t)) ((tag_id, _): Tag.t Link.t) (module DB: DB) =
  DB.exec add_post_tag_request (Some post_id, Some tag_id, url) |> flatten_error

let add_post_tags (post: t Link.t) tags db =
  Lwt_list.fold_left_s (function
      Error _ as e -> fun _ -> Lwt.return e
    | Ok () -> fun (tag, url) -> add_post_tag ?url post tag db
  ) (Ok ()) tags

let collect_post_tags
      (((post_id, _): t Link.t)) (module DB: DB)
  : ((Tag.t * string option) List.t, string) R.t =
  let* ls  = DB.collect_list collect_post_tags_request (Some post_id)
             |> flatten_error in
  let tags =
    List.map
      (fun (id, name', url) -> (Tag.{id; name=name'}, url))
      ls in
  Lwt_result.return tags

let add_post_mention (((post_id, _): t Link.t)) ((actor_id, _): Actor.t Link.t) (module DB: DB) =
  DB.exec add_post_mention_request (post_id, actor_id) |> flatten_error

let add_post_mentions (post: t Link.t) mentions db =
  Lwt_list.fold_left_s (function
      Error _ as e -> fun _ -> Lwt.return e
    | Ok () -> fun mention -> add_post_mention post mention db
  ) (Ok ()) mentions

let collect_post_mentions
      (((post_id, _): t Link.t)) (module DB: DB)
  : (Actor.t Link.t List.t, string) R.t =
  let* ls  = DB.collect_list collect_post_mentions_request post_id
             |> flatten_error in
  let mentions = List.map (fun id -> (id, Actor.resolve)) ls in
  Lwt_result.return mentions

let collect_post_feed ?offset ((post_id, _): Actor.t Link.t) (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_post_feed_request (post_id, post_id, post_id, post_id)
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_post_feed_offset_request
      (timestamp, post_id, post_id,
       (post_id, post_id, limit, offset))
    |> flatten_error

let collect_post_direct ?offset ((post_id, _): Actor.t Link.t) (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_post_direct_messages_request (post_id, post_id, post_id)
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_post_direct_messages_offset_request
      (timestamp, post_id, post_id, (post_id, limit, offset))
    |> flatten_error

let collect_post_whole_known_network ?offset (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_post_whole_known_network_request ()
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_post_whole_known_network_offset_request
      (timestamp, limit, offset)
    |> flatten_error

let collect_post_local_network ?offset (module DB: DB) =
  match offset with
  | None ->
    DB.collect_list collect_post_local_network_request ()
    |> flatten_error
  | Some (timestamp, limit, offset) ->
    DB.collect_list collect_post_local_network_offset_request
      (timestamp, limit, offset)
    |> flatten_error

let self t : t Link.t = t.id, resolve_post
let public_id t = t.public_id
let author t : Actor.t Link.t = t.author, Actor.resolve
let url t = t.url
let raw_data t = t.raw_text
let is_public t : bool = t.is_public
let summary t : string option = t.summary
let post_source t : string = t.post_source
let published t : Calendar.t = t.published
