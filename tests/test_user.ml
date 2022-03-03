open Bos
open Containers
module T = Testing_utils.Lwt.Make (struct let name = "user" end)
module Ty = Caqti_type.Std
module type DB = Caqti_lwt.CONNECTION
let (let+) x f = Lwt.bind x f
let (let@) x f = Lwt.(x >|= f)
let (let*) x f = Lwt.(x >|= function v -> f (Result.get_exn v))
let ret = Lwt.return_unit

let init_str = IO.with_in "../resources/schema.sql" IO.read_all
let test_db_path = Fpath.of_string "./test.db" |> Result.get_exn
let create_db_cmd = Cmd.(v "sqlite3" % "-init" % "../resources/schema.sql" % "./test.db")

let check_is_true vl = Alcotest.(check bool) "predicate is true" true vl
let check_is_false vl = Alcotest.(check bool) "predicate is false" false vl
let check_is_ok user = Alcotest.(check bool) "result is ok" true (Result.is_ok user)
let check_string_eq ~expected value = Alcotest.(check string) "string matches" expected value
let check_string_neq ~expected value = Alcotest.(check bool) "string does not match" false String.(expected = value)

let with_db f () =
  OS.File.delete ~must_exist:false test_db_path |> Result.get_exn;
  OS.Cmd.run create_db_cmd |> Result.get_exn;
  (* print_endline @@ "at line " ^ Int.to_string __LINE__; *)
  let+ connection = Caqti_lwt.connect (Uri.make ~scheme:"sqlite3" ~path:(Fpath.to_string test_db_path) ()) in
  begin
    match connection with
    | Ok _ -> ()
    |Error e -> failwith (Caqti_error.show e)
  end;
  let (module DB) = Result.get_exn connection in
  f (module DB : Caqti_lwt.CONNECTION)
;;

T.add_test "can create user" @@ with_db @@ fun db ->
let@ user = Database.User.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
check_is_ok user
;;

T.add_test "user's username matches " @@ with_db @@ fun db ->
let* user = Database.User.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
check_string_eq ~expected:"example-user" (Database.User.username user)
;;

T.add_test "user's password does not match" @@ with_db @@ fun db ->
let* user = Database.User.create_user ~username:"example-user" ~password:"areallygoodpasswordhere121" db in
check_string_neq ~expected:"areallygoodpasswordhere121"
  (Database.User.username user)
;;

T.add_test "user can login" @@ with_db @@ fun db ->
let username = "example-user" in
let password = "areallygoodpasswordhere121" in
let+ _ = Database.User.create_user ~username ~password db in
let@ user_again = Database.User.login_user ~username ~password db in
check_is_ok user_again
;;


let () = T.run ()
