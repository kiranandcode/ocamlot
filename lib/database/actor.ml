open Utils

type t =
  | Local of Local_user.t
  | Remote of Remote_user.t

let lookup_local_id_request =
  Caqti_request.find ~oneshot:false T.Std.int64 T.Std.int64 {| SELECT id FROM Actor WHERE local_id = ? |}

let lookup_remote_id_request =
  Caqti_request.find ~oneshot:false T.Std.int64 T.Std.int64 {| SELECT id FROM Actor WHERE remote_id = ? |}

let resolve_request =
  Caqti_request.find ~oneshot:false T.Std.int64 T.Std.(tup2 (option int64) (option int64))
    {| SELECT local_id, remote_id FROM Actor WHERE id = ? |}

let create_local_user_request =
  Caqti_request.exec ~oneshot:false T.Std.int64 {| INSERT OR IGNORE INTO Actor (local_id)  VALUES (?) |}

let create_remote_user_request =
  Caqti_request.exec ~oneshot:false T.Std.int64 {| INSERT OR IGNORE INTO Actor (remote_id)  VALUES (?) |}

let resolve id (module DB: DB) =
  let* result = DB.find resolve_request id |> flatten_error in
  match result with
  | Some local_id, _ ->
    let* local_user = Local_user.resolve_user local_id (module DB) in
    R.return (Local local_user)
  | _,  Some remote_id ->
    let* remote_user = Remote_user.resolve_remote_user remote_id (module DB) in
    R.return (Remote remote_user)
  | _ -> assert false
    
let of_local ((local_id, _): Local_user.t Link.t) (module DB: DB) =
  let* result = DB.find_opt lookup_local_id_request local_id |> flatten_error in
  match result with
  | Some id -> R.return (id, resolve)
  | None ->
    let* () = DB.exec create_local_user_request local_id |> flatten_error in
    let* id = DB.find lookup_local_id_request local_id |> flatten_error in
    R.return (id, resolve)

let of_remote ((remote_id, _): Remote_user.t Link.t) (module DB: DB) =
  let* result = DB.find_opt lookup_remote_id_request remote_id |> flatten_error in
  match result with
  | Some id -> R.return (id, resolve)
  | None ->
    let* () = DB.exec create_remote_user_request remote_id |> flatten_error in
    let* id = DB.find lookup_remote_id_request remote_id |> flatten_error in
    R.return (id, resolve)
