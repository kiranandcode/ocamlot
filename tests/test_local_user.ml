open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "user" end);;

T.add_test "can create user" @@ with_db @@ fun db ->
let+ user = Database.LocalUser.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
ret begin
check_is_ok user
end
;;

T.add_test "user's fields matches " @@ with_db @@ fun db ->
let* user = Database.LocalUser.create_user
              ~about:"I am teh pengu1nz of d00mz."
              ~username:"example-user"
              ~password:"areallygoodpasswordhere121" db in
ret begin
check_string_eq ~expected:"example-user" (Database.LocalUser.username user);
check_is_false (Database.LocalUser.is_admin user);
check_is_false (Database.LocalUser.manually_accept_follows user);
Alcotest.(check (option string)) "about matches"
  (Some "I am teh pengu1nz of d00mz.")
  (Database.LocalUser.about user)
end
;;

T.add_test "user's password does not match" @@ with_db @@ fun db ->
let* user = Database.LocalUser.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
ret begin
check_string_neq ~expected:"areallygoodpasswordhere121"
  (Database.LocalUser.username user)
end
;;

T.add_test "user can login" @@ with_db @@ fun db ->
let username = "example-user" in
let password = "areallygoodpasswordhere121" in
let* _ = Database.LocalUser.create_user ~username ~password db in
let+ user_again = Database.LocalUser.login_user ~username ~password db in
ret begin
check_is_ok user_again
end
;;

T.add_test "can update about strings" @@ with_db @@ fun db ->
let username = "example-user" in
let password = "areallygoodpasswordhere121" in
let* user = Database.LocalUser.create_user ~username ~password db
          >|= Database.LocalUser.self in
let* () = Database.LocalUser.update_about user "I am teh penguinz of d00m!." db in
let* user = Database.LocalUser.login_user ~username ~password db in
ret begin
  Alcotest.(check (option string)) "about matches"
    (Some "I am teh penguinz of d00m!.")
    (Option.bind user Database.LocalUser.about)
end
;;

T.add_test "can update about admin" @@ with_db @@ fun db ->
let username = "example-user" in
let password = "areallygoodpasswordhere121" in
let* user = Database.LocalUser.create_user ~username ~password db
          >|= Database.LocalUser.self in
let* () = Database.LocalUser.update_is_admin user true db in
let* user = Database.LocalUser.login_user ~username ~password db
  >|= Option.get in
ret begin
  check_is_true
    (Database.LocalUser.is_admin user)
end
;;

T.add_test "can update manually accepts followers" @@ with_db @@ fun db ->
let username = "example-user" in
let password = "areallygoodpasswordhere121" in
let* user = Database.LocalUser.create_user ~username ~password db
          >|= Database.LocalUser.self in
let* () = Database.LocalUser.update_manually_accept_follows user true db in
let* user = Database.LocalUser.login_user ~username ~password db
  >|= Option.get in
ret begin
  check_is_true
    (Database.LocalUser.manually_accept_follows user)
end
;;

let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
