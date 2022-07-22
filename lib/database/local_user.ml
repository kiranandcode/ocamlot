open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"


(* see ./resources/schema.sql:LocalUser *)
type%sql.generate t = SQL [@schema "LocalUser"]

let build_enc (to_cstr, from_cstr)  =
  let encode v = Ok (Cstruct.to_string (to_cstr v)) in
  let decode v =
    from_cstr (Cstruct.of_string v) |> Result.map_err (function `Msg str -> str) in
  T.redacted (T.Std.custom ~encode ~decode T.Std.string)

let display_name user =
  user.display_name
  |> Option.value ~default:user.username 

let pubkey : X509.Public_key.t T.t =
  build_enc X509.Public_key.(encode_pem, decode_pem)

let privkey : X509.Private_key.t T.t =
  build_enc X509.Private_key.(encode_pem, decode_pem)

let t : t T.t =
  let encode {id;username;password_hash;display_name; about;
              manually_accept_follows; is_admin; pubkey; privkey} =
    Ok (id, username, password_hash,
        (display_name, about, manually_accept_follows,
         (is_admin, pubkey, privkey))) in
  let decode (id, username, password_hash,
              (display_name, about, manually_accept_follows,
               (is_admin, pubkey, privkey))) =
    Ok {id;username;password_hash;display_name; about;
        manually_accept_follows; is_admin; pubkey; privkey} in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 string string
             (tup4 (option string) (option string) bool
                (tup3 bool pubkey privkey)))

let create_user_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  tup4 string string (option string) (tup4 bool bool pubkey privkey)
    -->. unit @:- {|
INSERT INTO LocalUser (username, password, about, manually_accept_follows, is_admin, pubkey, privkey)
 VALUES (?, ?, ?, ?, ?, ?, ?) |}

let find_user_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->! t @:- {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE username = ?  |}

let resolve_user_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->! t @:- {|
SELECT id, username, password, display_name, about, manually_accept_follows, is_admin, pubkey, privkey
FROM LocalUser
WHERE id = ?  |}


let update_about_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  tup2 string int64 -->. unit @:- {|
UPDATE LocalUser
SET about = ?
WHERE id = ?
|}

let update_is_admin_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  tup2 bool int64 -->. unit @:- {|
UPDATE LocalUser
SET is_admin = ?
WHERE id = ?
|}

let update_manually_accept_follows_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  tup2 bool int64 -->. unit @:- {|
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
  DB.exec update_about_request (about, user_id)

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

