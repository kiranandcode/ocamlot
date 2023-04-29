open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "activity" end);;

T.add_test "can create activity" @@ with_db @@ fun db ->
let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "data" } |} in
let+ user = Database.Activity.create ~id ~data db in
ret (check_is_ok user)
;;

T.add_test "can lookup activity by id" @@ with_db @@ fun db ->
let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "data" } |} in
let+ _ = Database.Activity.create ~id ~data db in
let* user' = Database.Activity.find id db in
ret begin
  check_is_some user'
end
;;

T.add_test "looked up activity data matches" @@ with_db @@ fun db ->
let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "data" } |} in
let+ _ = Database.Activity.create ~id ~data db in
let* user = Database.Activity.find id db in
let user_data = Option.get user |> Database.Activity.data in
ret begin
  check_string_eq ~expected:(Yojson.Safe.to_string data)
    (Yojson.Safe.to_string user_data)
end
;;

T.add_test "looked up activity data matches with multiple" @@ with_db @@ fun db ->
let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "another" } |} in
let+ _ = Database.Activity.create ~id ~data db in

let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "data" } |} in

let+ _ = Database.Activity.create ~id ~data db in
let* user = Database.Activity.find id db in
let user_data = Option.get user |> Database.Activity.data in
ret begin
check_string_eq ~expected:(Yojson.Safe.to_string data)
  (Yojson.Safe.to_string user_data)
end
;;


T.add_test "can update activity data" @@ with_db @@ fun db ->
let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "data" } |} in
let+ user = Database.Activity.create ~id ~data db in
let user = Result.get_ok user in
let data2 = Yojson.Safe.from_string {| { "example": "data2" } |} in
let+ _ = Database.Activity.update user data2 db in
let* user = Database.Activity.find id db in
let user_data = Option.get user |> Database.Activity.data in
ret begin
  check_string_eq ~expected:(Yojson.Safe.to_string data2)
    (Yojson.Safe.to_string user_data)
end
;;

T.add_test "updating activity data doesn't change others" @@ with_db @@ fun db ->
let old_id = Database.Activity.fresh_id () in
let old_data = Yojson.Safe.from_string {| { "example": "old_data" } |} in
let+ _ = Database.Activity.create ~id:old_id ~data:old_data db in

let id = Database.Activity.fresh_id () in
let data = Yojson.Safe.from_string {| { "example": "data" } |} in
let+ user = Database.Activity.create ~id ~data db in

let user = Result.get_ok user in
let data2 = Yojson.Safe.from_string {| { "example": "data2" } |} in
let+ _ = Database.Activity.update user data2 db in
let* user = Database.Activity.find old_id db in
let user_data = Option.get user |> Database.Activity.data in
ret begin
  check_string_eq ~expected:(Yojson.Safe.to_string old_data)
    (Yojson.Safe.to_string user_data)
end
;;



let () =
  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna);
  T.run ()
