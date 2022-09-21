[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"


(* see ./resources/schema.sql:LocalUser *)
type t = Types.local_user
let t = Types.local_user

let display_name (user: t) =
  user.display_name
  |> Option.value ~default:user.username

let%sql.query find_user_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE username = ?  |}

let resolve_user =
  let%sql.query resolve_user_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE id = ?  |} in
  fun id (module DB: DB) ->
    DB.find resolve_user_request id |> flatten_error

let create_user =
  let%sql.query create_user_request = {|
INSERT INTO LocalUser (username, password, about, manually_accept_follows, is_admin, pubkey, privkey)
 VALUES (?, ?, ?, ?, ?, ?, ?) |} in
  fun  ?about ?(is_admin=false) ?(manually_accepts_follows=false)
    ~username ~password (module DB: DB) ->

    let+ password_hash = Password.hash ~pwd:password in
    let priv_key = X509.Private_key.generate `RSA in
    let pub_key = X509.Private_key.public priv_key in
    let* () = flatten_error @@
      DB.exec create_user_request
        (username, password_hash, about,
         (manually_accepts_follows, is_admin, pub_key, priv_key)) in
    flatten_error @@ DB.find find_user_request username

let login_user ~username ~password (module DB: DB) =
  let* user = flatten_error @@ DB.find_opt find_user_request username in
  match user with
  | None -> R.return None
  | Some user ->
    let+ password_is_correct = Password.verify user.password_hash ~pwd:password in
    R.return (if password_is_correct
              then Some user
              else None)

let lookup_user ~username (module DB: DB) =
  let* user = flatten_error @@ DB.find_opt find_user_request username in
  R.return user

let lookup_user_exn ~username (module DB: DB) =
  let* user = flatten_error @@ DB.find find_user_request username in
  R.return user

let update_about =
  let%sql.query update_about_request = {|
UPDATE LocalUser
SET about = ?
WHERE id = ?
|} in
  fun ((user_id, _): t Link.t) about (module DB: DB) ->
    DB.exec update_about_request (Some about, user_id) |> flatten_error

let update_manually_accept_follows =
  let%sql.query update_manually_accept_follows_request = {|
UPDATE LocalUser
SET manually_accept_follows = ?
WHERE id = ?
|} in
  fun  ((user_id, _): t Link.t) manually_accept_follows (module DB: DB) ->
    DB.exec update_manually_accept_follows_request
      (manually_accept_follows, user_id) |> flatten_error

let update_is_admin =
  let%sql.query update_is_admin_request = {|
UPDATE LocalUser
SET is_admin = ?
WHERE id = ?
|} in
  fun  ((user_id, _): t Link.t) is_admin (module DB: DB) ->
    DB.exec update_is_admin_request (is_admin, user_id) |> flatten_error

let collect_local_users =
  let%sql.query collect_local_users_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
ORDER BY username DESC
|} in
  let%sql.query collect_local_users_offset_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
ORDER BY username DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list collect_local_users_request ()
      |> flatten_error
    | Some (limit, offset) ->
      DB.collect_list collect_local_users_offset_request
        (limit, offset)
      |> flatten_error

let find_local_users =
  let%sql.query find_local_users_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE username LIKE ? OR display_name LIKE ?
ORDER BY username DESC
|} in
  let%sql.query find_local_users_offset_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE username LIKE ? OR display_name LIKE ?
ORDER BY username DESC
LIMIT ? OFFSET ?
|} in
  fun ?offset query (module DB: DB) ->
    match offset with
    | None ->
      DB.collect_list find_local_users_request (query, query)
      |> flatten_error
    | Some (limit, offset) ->
      DB.collect_list find_local_users_offset_request
        (query, query, limit, offset)
      |> flatten_error

let self (user: t) : t Link.t = (user.id, resolve_user)
let username (user: t) = user.username
let about (user: t) = user.about
let pubkey (user: t) =
  user.pubkey
  |> X509.Public_key.encode_pem
  |> Cstruct.to_string
let is_admin (user: t) = user.is_admin
let manually_accept_follows (user: t) = user.manually_accept_follows
let privkey (user: t) = user.privkey

