[@@@warning "-33"]
open Containers
open Utils

(* see ./resources/schema.sql:Post *)
type t = {
  id: int64;                              (* unique internal id of the tag *)
  name: string;                           (* tag value *)
}

let t =
  let encode { id; name } =
    Ok (id, name) in
  let decode (id, name) =
    Ok { id; name } in
  T.Std.custom ~encode ~decode
    T.Std.(tup2 int64 string)

let create_tag_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  string -->. unit @:- {|
INSERT OR IGNORE INTO Tags (tag_name) VALUES (?)
|}

let find_tag_by_name_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in

  string -->! t @:-
    {|
SELECT tag_id, tag_name
FROM Tags
WHERE tag_name = ?
|}

let resolve_tag_request =
  let open Caqti_type.Std in
  let open Caqti_request.Infix in
  int64 -->! t @:-
    {|
SELECT tag_id, tag_name
FROM Tags
WHERE tag_id = ?
|}

let resolve id (module DB: DB) =
  DB.find resolve_tag_request id |> flatten_error

let create name (module DB: DB) =
  let* () = DB.exec create_tag_request name
            |> flatten_error in
  DB.find find_tag_by_name_request name
  |> flatten_error

let find name (module DB: DB) =
  DB.find_opt find_tag_by_name_request name
  |> flatten_error

let find_exn name (module DB: DB) =
  DB.find find_tag_by_name_request name
  |> flatten_error


let self (t: t) : t Link.t = (t.id, resolve)
let name (t: t) : string = t.name
