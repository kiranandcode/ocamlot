module T = Caqti_type
module type DB = Caqti_lwt.CONNECTION
module R = Lwt_result
module Calendar = CalendarLib.Calendar

let (let*) x f = R.bind x f
let (let+) x f = R.bind (R.lift x) f
let flatten_error err = R.map_error (fun err ->  Caqti_error.show err) err


type timestamp = Calendar.t
let timestamp = Caqti_type_calendar.ctime


type uuid = Uuidm.t

let uuid: Uuidm.t T.t =
  let encode id = Ok (Uuidm.to_string id) in
  let decode id = Uuidm.of_string id |> Option.to_result ~none:"uuid failed to decode" in
  T.Std.custom ~encode ~decode
    T.Std.(string)

type yojson = Yojson.Safe.t

let yojson: Yojson.Safe.t T.t =
  let encode data = Ok (Yojson.Safe.to_string data) in
  let decode data =
    try
      Ok (Yojson.Safe.from_string data)
    with exn ->
      Error (Printexc.to_string exn) in
  T.Std.custom ~encode ~decode T.Std.(string)

let build_enc (to_cstr, from_cstr)  =
  let encode v = Ok (Cstruct.to_string (to_cstr v)) in
  let decode v =
    from_cstr (Cstruct.of_string v) |> Result.map_error (function `Msg str -> str) in
  T.redacted (T.Std.custom ~encode ~decode T.Std.string)

type pubkey = X509.Public_key.t

let pubkey : X509.Public_key.t T.t =
  build_enc X509.Public_key.(encode_pem, decode_pem) 

type privkey = X509.Private_key.t

let privkey : X509.Private_key.t T.t =
  build_enc X509.Private_key.(encode_pem, decode_pem)
