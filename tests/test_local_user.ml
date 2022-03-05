open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "user" end);;

T.add_test "can create user" @@ with_db @@ fun db ->
let@ user = Database.LocalUser.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
check_is_ok user
;;

T.add_test "user's username matches " @@ with_db @@ fun db ->
let* user = Database.LocalUser.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
check_string_eq ~expected:"example-user" (Database.LocalUser.username user)
;;

T.add_test "user's password does not match" @@ with_db @@ fun db ->
let* user = Database.LocalUser.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
check_string_neq ~expected:"areallygoodpasswordhere121"
  (Database.LocalUser.username user)
;;

T.add_test "user can login" @@ with_db @@ fun db ->
let username = "example-user" in
let password = "areallygoodpasswordhere121" in
let+ _ = Database.LocalUser.create_user ~username ~password db in
let@ user_again = Database.LocalUser.login_user ~username ~password db in
check_is_ok user_again
;;


let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
