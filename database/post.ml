[@@@warning "-33"]
open Containers
module Error = Lib.Error
module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type
module L = Caqti_lwt

type id = int
type t = {
  message_id: id;
  posted_at: Ptime.t;
  author: User.id;
  message: string;
}

let t =
  let decode (message_id, posted_at, author,message) =
    Ok {message_id; posted_at; author;message} in
  let encode {message_id; posted_at; author;message} =
    Ok (message_id, posted_at, author,message) in
  T.custom ~encode ~decode T.(tup4 int ptime int string)

module DB = struct

  let migrate (module DB: DB) =
    let migrate =
      R.exec T.unit {|
        CREATE TABLE IF NOT EXISTS posts (
          message_id SERIAL NOT NULL PRIMARY KEY,
          posted_at VARCHAR NOT NULL,
          author_id INT,
          message VARCHAR NOT NULL,
          CONSTRAINT fk_author
            FOREIGN KEY(author_id) 
            REFERENCES users(id)
            ON DELETE SET NULL
        ) |} in
    DB.exec migrate () |> Error.wrap_caqti_error

  let rollback (module DB: DB) =
    let rollback =
      R.exec T.unit {|
        DROP TABLE IF EXISTS posts
     |} in
    DB.exec rollback () |> Error.wrap_caqti_error

end
