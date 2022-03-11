open Containers
open Utils

(* see ./resources/schema.sql:Activity *)
type t = {
  id: Uuidm.t;                           (* UNIQUE public UUID of activity *)
  data: Yojson.Safe.t;                   (* raw json data stored at UUID *)
}

type id = Uuidm.t

let uuid: Uuidm.t T.t =
  let encode id = Ok (Uuidm.to_string id) in
  let decode id = Uuidm.of_string id |> Option.to_result "uuid failed to decode" in
  T.Std.custom ~encode ~decode
    T.Std.(string)

let yojson: Yojson.Safe.t T.t =
  let encode data = Ok (Yojson.Safe.to_string data) in
  let decode data =
    try
      Ok (Yojson.Safe.from_string data)
    with exn ->
      Error (Printexc.to_string exn) in
  T.Std.custom ~encode ~decode T.Std.(string)


let t : t T.t =
  let encode {id; data} = Ok (id, data) in
  let decode (id, data) = Ok {id;data} in
  T.Std.custom ~encode ~decode
    T.Std.(tup2 uuid yojson)


let create_activity_request =
  Caqti_request.exec ~oneshot:false t {| INSERT OR IGNORE INTO Activity (id, raw_data)  VALUES (?, ?) |}

let find_activity_request =
  Caqti_request.find ~oneshot:false uuid t {| SELECT id, raw_data FROM Activity WHERE id = ?  |}

let update_activity_request =
  Caqti_request.exec ~oneshot:false T.Std.(tup2 yojson uuid) {| UPDATE OR IGNORE Activity SET raw_data = ? WHERE id = ? |}

let data {id=_;data} = data
let id {id;data=_} = id

let url config id = (Configuration.Url.activity_endpoint config (Uuidm.to_string ~upper:false id))

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
