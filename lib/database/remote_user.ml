[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:RemoteUser *)
type t = Types.remote_user

let%sql.query lookup_remote_user_by_address_request = {|
SELECT
    RemoteUser.id,
    RemoteUser.username,
    RemoteUser.instance_id,
    RemoteUser.display_name,
    RemoteUser.url,
    RemoteUser.inbox,
    RemoteUser.outbox,
    RemoteUser.followers,
    RemoteUser.following,
    RemoteUser.summary,
    RemoteUser.public_key_pem
FROM RemoteUser JOIN RemoteInstance on RemoteUser.instance_id = RemoteInstance.id 
WHERE RemoteInstance.url = ? AND RemoteUser.username = ?
|}

let%sql.query lookup_remote_user_by_url_request = 
  {| SELECT id, username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem FROM RemoteUser WHERE url = ?  |}


let resolve_remote_user =
  let%sql.query resolve_remote_user_request =
    {| SELECT id, username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem FROM RemoteUser WHERE id = ?  |} in
  fun id (module DB: DB) ->
    DB.find resolve_remote_user_request id |> flatten_error

let create_remote_user =
  let%sql.query create_remote_user_request =
    {|
INSERT INTO RemoteUser (username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem)  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
|} in
  fun ?display_name ?inbox ?outbox ?followers ?following ?summary
    ~username ~instance:((instance, _): Remote_instance.t Link.t) ~url ~public_key_pem
    (module DB: DB) ->
    let* () = DB.exec create_remote_user_request (username, instance, display_name,
                                                  (url, inbox, outbox,
                                                   (followers, following, summary, public_key_pem)))
              |> flatten_error in
    flatten_error @@ DB.find lookup_remote_user_by_url_request url

let lookup_remote_user_by_url url (module DB: DB) =
  flatten_error @@ DB.find_opt lookup_remote_user_by_url_request url
let lookup_remote_user_by_url_exn url (module DB: DB) =
  flatten_error @@ DB.find lookup_remote_user_by_url_request url


let lookup_remote_user_by_address ~username ~domain (module DB: DB) =
  flatten_error @@ DB.find_opt lookup_remote_user_by_address_request (domain, username)
let lookup_remote_user_by_address_exn ~username ~domain (module DB: DB) =
  flatten_error @@ DB.find lookup_remote_user_by_address_request (domain, username)

let get_known_remote_actors =
  let%sql.query retrieve_known_user_list_reqest = {|
SELECT RemoteUser.username, RemoteInstance.url, RemoteUser.url FROM RemoteUser 
JOIN RemoteInstance on RemoteUser.instance_id = RemoteInstance.id
|} in
  fun (module DB: DB) ->
    DB.collect_list retrieve_known_user_list_reqest () |> flatten_error

let collect_remote_users =
  let%sql.query collect_remote_users_request = {|
SELECT
    RU.id,
    RU.username,
    RU.instance_id,
    RU.display_name,
    RU.url,
    RU.inbox,
    RU.outbox,
    RU.followers,
    RU.following,
    RU.summary,
    RU.public_key_pem,
    RI.url
FROM RemoteUser AS RU
JOIN RemoteInstance AS RI on RemoteUser.instance_id = RemoteInstance.id
ORDER BY (RU.display_name, RI.url)
|} in
  let%sql.query collect_remote_users_offset_request = {|
SELECT
    RU.id,
    RU.username,
    RU.instance_id,
    RU.display_name,
    RU.url,
    RU.inbox,
    RU.outbox,
    RU.followers,
    RU.following,
    RU.summary,
    RU.public_key_pem,
    RI.url
FROM RemoteUser AS RU
JOIN RemoteInstance AS RI on RemoteUser.instance_id = RemoteInstance.id
ORDER BY RU.display_name DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list collect_remote_users_request () |> flatten_error
    | Some (limit, offset) ->
      DB.collect_list collect_remote_users_offset_request (limit, offset) |> flatten_error

let collect_remote_users_following =
  let%sql.query collect_remote_users_request = {|
-- select users from remote users
SELECT
    RU.id,
    RU.username,
    RU.instance_id,
    RU.display_name,
    RU.url,
    RU.inbox,
    RU.outbox,
    RU.followers,
    RU.following,
    RU.summary,
    RU.public_key_pem
FROM RemoteUser as RU
JOIN Actor as A ON A.remote_id = RU.id
WHERE
  -- where there exists a follow
  EXISTS (
      SELECT *
     FROM Follows AS F
     -- where the author is the remote user
     WHERE F.author_id = A.id
       -- the target is our guy
       AND F.target_id = ?
       -- and the follow is not pending
       AND F.pending = FALSE)
ORDER BY RU.id ASC
|} in
  let%sql.query collect_remote_users_offset_request = {|
-- select users from remote users
SELECT
    RU.id,
    RU.username,
    RU.instance_id,
    RU.display_name,
    RU.url,
    RU.inbox,
    RU.outbox,
    RU.followers,
    RU.following,
    RU.summary,
    RU.public_key_pem
FROM RemoteUser as RU
JOIN Actor as A ON A.remote_id = RU.id
WHERE
  -- where there exists a follow
  EXISTS (
      SELECT *
     FROM Follows AS F
     -- where the author is the remote user
     WHERE F.author_id = A.id
       -- the target is our guy
       AND F.target_id = ?
       -- and the follow is not pending
       AND F.pending = FALSE)
ORDER BY RU.id ASC
LIMIT ? OFFSET ?
|} in
  fun ?offset
    ((local_user, _): Local_user.t Link.t) (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list collect_remote_users_request local_user |> flatten_error
    | Some (limit, offset) ->
      DB.collect_list collect_remote_users_offset_request (local_user, limit, offset)
      |> flatten_error

let find_remote_users =
  let%sql.query find_remote_users_request = {|
SELECT id, username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem 
FROM RemoteUser
WHERE username LIKE ? OR display_name LIKE ?
ORDER BY username DESC
|} in
  let%sql.query find_remote_users_offset_request = {|
SELECT id, username, instance_id, display_name, url, inbox, outbox, followers, following, summary, public_key_pem 
FROM RemoteUser
WHERE username LIKE ? OR display_name LIKE ?
ORDER BY username DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset query (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list find_remote_users_request (query, query)
      |> flatten_error
    | Some (limit, offset) ->
      DB.collect_list find_remote_users_offset_request
        (query, query, limit, offset)
      |> flatten_error

let self (t: t) : t Link.t = t.id, resolve_remote_user
let username (t: t) = t.username
let instance (t: t) : Remote_instance.t Link.t = t.instance_id, Remote_instance.resolve_instance
let display_name (t: t) = t.display_name |> Option.value ~default:t.username
let url (t: t) = t.url
let inbox (t: t) : Uri.t =
  t.inbox
  |> Option.map Uri.of_string
  |> function
  | Some v -> v
  | None ->
    let user = t.url |> Uri.of_string in
    let path = user |> Uri.path in
    Uri.with_path user (path ^ "/inbox")

let outbox (t: t) : Uri.t option = t.outbox |> Option.map Uri.of_string
let followers (t: t) : Uri.t option = t.followers |> Option.map Uri.of_string
let following (t: t) : Uri.t option = t.following |> Option.map Uri.of_string
let summary (t: t) : string option = t.summary
let public_key (t: t) : X509.Public_key.t = t.public_key_pem |> Cstruct.of_string |> X509.Public_key.decode_pem |> Result.get_exn
