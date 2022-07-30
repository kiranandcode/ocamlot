[@@@warning "-33"]
open Containers
open Utils

let () = declare_schema "../../resources/schema.sql"

(* see ./resources/schema.sql:Tag *)
type t = Types.tag
let t = Types.tag


let%sql.query find_tag_by_name_request =
  {|
SELECT tag_id, tag_name
FROM Tags
WHERE tag_name = ?
|}

let resolve id (module DB: DB) =
  let%sql.query resolve_tag_request =
    {|
SELECT tag_id, tag_name
FROM Tags
WHERE tag_id = ?
|} in
  DB.find resolve_tag_request id |> flatten_error

let create name (module DB: DB) =
  let%sql.query create_tag_request = {|
INSERT OR IGNORE INTO Tags (tag_name) VALUES (?)
|} in
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
