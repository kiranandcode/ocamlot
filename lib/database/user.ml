[@@@warning "-26-27"]
open Containers
module T = Caqti_type
module type DB = Caqti_lwt.CONNECTION
module R = Lwt_result

(* see ./resources/schema.sql:users *)
type t = {
  id: int64;                           (* UNIQUE Id of user *)
  username: string;                    (* username (fixed) *)
  password_hash: string;               (* password (hash) *)
  display_name: string option;         (* display name *)
  about: string option;                (* about text *)
  pubkey: X509.Public_key.t;                  (* public key *)
  privkey: X509.Private_key.t;                (* private key *)
}

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
  let encode {id;username;password_hash;display_name; about; pubkey; privkey} =
    Ok (id, username, password_hash, (display_name, about, pubkey, privkey)) in
  let decode (id, username, password_hash, (display_name, about, pubkey, privkey)) =
    Ok {id;username;password_hash;display_name; about; pubkey; privkey} in
  T.Std.custom ~encode ~decode
    T.Std.(tup4 int64 string string (tup4 (option string) (option string) pubkey privkey))


let (let*) x f = R.bind x f
let (let+) x f = R.bind (R.lift x) f
let flatten_error err = R.map_err (fun err ->  Caqti_error.show err) err

let create_user_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup4 string string pubkey privkey) {| INSERT INTO users (username, password, pubkey, privkey)  VALUES (?, ?, ?, ?) |}

let find_user_request =
  Caqti_request.find ~oneshot:false T.Std.string t {| SELECT id, username, password, display_name, about, pubkey, privkey FROM users WHERE username = ?  |}


let create_user ~username ~password (module DB: DB) =
  let+ password_hash = Password.hash ~pwd:password in
  let priv_key = X509.Private_key.generate `RSA in
  let pub_key = X509.Private_key.public priv_key in
  let* () = flatten_error @@ DB.exec create_user_request (username, password_hash, pub_key, priv_key) in
  flatten_error @@ DB.find find_user_request username

let login_user ~username ~password (module DB: DB) =
  let+ password_hash = Password.hash ~pwd:password in
  let* user = flatten_error @@ DB.find_opt find_user_request username in
  match user with
  | None -> R.return None
  | Some user ->
    let+ password_is_correct = Password.verify user.password_hash ~pwd:password in
    R.return (if password_is_correct
              then Some user
              else None)

let lookup_user ~username (module DB: DB) =
  let* user = flatten_error @@ DB.find find_user_request username in
  R.return user

let username user = user.username

let pubkey user =
  user.pubkey
  |> X509.Public_key.encode_pem
  |> Cstruct.to_string
