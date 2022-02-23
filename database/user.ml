[@@@warning "-33"]
open Containers
module Error = Lib.Error
module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type
module L = Caqti_lwt

type id = int

type t = {
  id: id;
  username: string;
  password_hash: string;
  email: string;
}

let t =
  let decode (id,username,password_hash,email) =
    Ok {id;username;password_hash;email} in
  let encode {id;username;password_hash;email} =
    Ok (id,username,password_hash,email) in
  T.custom ~encode ~decode T.(tup4 int string string string)

module DB = struct

  let migrate (module DB: DB) =
    let migrate =
      R.exec T.unit {|
        CREATE TABLE IF NOT EXISTS users (
          id SERIAL NOT NULL PRIMARY KEY,
          username VARCHAR NOT NULL UNIQUE,
          password_hash VARCHAR NOT NULL,
          email VARCHAR NOT NULL
        ) |} in
    DB.exec migrate () |> Error.wrap_caqti_error

  let rollback (module DB: DB) =
    let rollback =
      R.exec T.unit {|
        DROP TABLE IF EXISTS users
     |} in
    DB.exec rollback () |> Error.wrap_caqti_error


  let create_user (module DB: DB) ~username ~password ~email =
    let query =
      R.find_opt
        T.(tup3 string string string)
        t
        {|
      INSERT INTO users (username,password_hash,email)
      VALUES (?, ?, ?)
      RETURNING id, username, password_hash, email
    |} in
    let open Lwt_result.Syntax in
    (* sanitize username, sanitize email *)
    let* password_hash =
      Lwt_result.lift @@ Lib.Password.hash ~pwd:password in
    (DB.find_opt query (username, password_hash, email))
    |> Error.wrap_caqti_error

  let find_user (module DB: DB) ~username =
    let query =
      R.find_opt T.string t
        {| SELECT * FROM users WHERE username = ? |} in
    let open Lwt_result.Syntax in
    (* sanitize username, sanitize email *)
    (DB.find_opt query username) |> Error.wrap_caqti_error

  let login db ~username ~password =
    let open Lwt_result.Syntax in
    let* result = find_user db ~username in
    match result with
    | None -> Lwt_result.return None
    | Some {password_hash;_} as user ->
      let* password_correct =
        Lwt_result.lift @@
        Lib.Password.verify password_hash ~pwd:password in
      if password_correct
      then Lwt_result.return user
      else Lwt_result.return None
end
