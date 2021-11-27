module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type
module L = Caqti_lwt

type t = {
  id: string;
  username: string;
  password_hash: string;
  email: string;
}

let t =
  let encode {id;username;password_hash;email} = Ok (id,username,password_hash,email) in
  let decode (id,username,password_hash,email) = Ok {id;username;password_hash;email} in
  let rep = T.(tup4 string string string string) in
  T.custom ~encode ~decode rep

module DB = struct

  let create =
    R.exec T.unit {|
CREATE TABLE "users" (
  id SERIAL PRIMARY KEY,
  username VARCHAR NOT NULL,
  password_hash VARCHAR NOT NULL,
  email VARCHAR
)
|}

  
end
