[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"


(* see ./resources/schema.sql:LocalUser *)
type%sql.generate t = SQL [@schema "LocalUser"]

let display_name user =
  user.display_name
  |> Option.value ~default:user.username

let%sql.query create_user_request = {|
INSERT INTO LocalUser (username, password, about, manually_accept_follows, is_admin, pubkey, privkey)
 VALUES (?, ?, ?, ?, ?, ?, ?) |}

let%sql.query find_user_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE username = ?  |}

let%sql.query resolve_user_request = {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE id = ?  |}


let%sql.query update_about_request = {|
UPDATE LocalUser
SET about = ?
WHERE id = ?
|}

let%sql.query update_is_admin_request = {|
UPDATE LocalUser
SET is_admin = ?
WHERE id = ?
|}

let%sql.query update_manually_accept_follows_request = {|
UPDATE LocalUser
SET manually_accept_follows = ?
WHERE id = ?
|}


let resolve_user id (module DB: DB) = DB.find resolve_user_request id |> flatten_error

let create_user ?about ?(is_admin=false) ?(manually_accepts_follows=false)
      ~username ~password (module DB: DB) =
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

let update_about ((user_id, _): t Link.t) about (module DB: DB) =
  flatten_error @@
  DB.exec update_about_request (Some about, user_id)

let update_manually_accept_follows
      ((user_id, _): t Link.t) manually_accept_follows (module DB: DB) =
  flatten_error @@
  DB.exec update_manually_accept_follows_request
    (manually_accept_follows, user_id)

let update_is_admin ((user_id, _): t Link.t) is_admin (module DB: DB) =
  flatten_error @@
  DB.exec update_is_admin_request (is_admin, user_id)

let self user : t Link.t = (user.id, resolve_user)
let username user = user.username
let about user = user.about
let pubkey user =
  user.pubkey
  |> X509.Public_key.encode_pem
  |> Cstruct.to_string
let is_admin user = user.is_admin
let manually_accept_follows user = user.manually_accept_follows
let privkey user = user.privkey

