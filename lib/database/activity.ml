[@@@warning "-36-39-33"]
let () = declare_schema "../../resources/schema.sql"
open Containers
open Utils

type id = Uuidm.t

(* see ./resources/schema.sql:Activity *)
type%sql.generate t = SQL [@schema "Activity"]

let%sql.query create_activity_request =
  {| INSERT OR IGNORE INTO Activity (id, raw_data)  VALUES (?, ?) |}

let%sql.query find_activity_request =
 {| SELECT id, raw_data FROM Activity WHERE id = ?  |}

let%sql.query update_activity_request =
  {| UPDATE OR IGNORE Activity SET raw_data = ? WHERE id = ? |}

let data {id=_;data} = data
let id {id;data=_} = id

let url config id = (Configuration.Url.activity_endpoint config (Uuidm.to_string ~upper:false id))

let id_to_string str = Uuidm.to_string str
let id_from_string str = Uuidm.of_string str

let fresh_id () =
  let bytes = Mirage_crypto_rng.generate 32 |> Cstruct.to_bytes in
  let id = Uuidm.v4 bytes in
  id

let create ~id ~data (module DB: DB) =
  let res = {id;data} in
  let* () = flatten_error @@ DB.exec create_activity_request res in
  Lwt.return_ok res

let find id (module DB: DB) =
  flatten_error @@ DB.find_opt find_activity_request id

let find_exn id (module DB: DB) =
  flatten_error @@ DB.find find_activity_request id

let update {id;data=_} data (module DB: DB) =
  let* () = flatten_error @@ DB.exec update_activity_request (data, id) in
  Lwt.return_ok {id;data}
