open Containers
open Lwt_result.Syntax
module R = Caqti_request
module T = Caqti_type
module User = Database.User
module Post = Database.Post
module type DB = Caqti_lwt.CONNECTION

let url = "postgresql://test:password@localhost:5433/test"

let wrap_caqti_error =
  Lwt_result.map_err (fun err -> Lib.Error.caqti err)

let with_transaction f (module DB: DB)  =
  let* () = DB.exec (R.exec  T.unit {| BEGIN TRANSACTION |}) ()
          |> wrap_caqti_error in
  let* result = f (module DB : DB) in
  let* () = DB.exec (R.exec  T.unit {| ROLLBACK |}) ()
          |> wrap_caqti_error in
  Lwt_result.return result

let with_db_connection f =
  let cpool = match
      Caqti_lwt.connect_pool ~max_size:10 (Uri.of_string url)
    with
    | Ok pool -> pool
    | Error err -> failwith (Caqti_error.show err) in
  Caqti_lwt.Pool.use (with_transaction f) cpool

let handle_error res =
  Lwt.map (function
      Ok () -> ()
    | Error err ->
      failwith (Lib.Error.show err)
  ) res

let with_database f conn =
  let* () = User.DB.migrate conn in
  f conn

let database_test f =
  with_db_connection (with_database f) |> handle_error


let can_migrate_user_database () =
  with_db_connection (fun db ->
    (User.DB.migrate db)
  )
  |> handle_error

let can_rollback_user_database () =
  with_db_connection (fun db ->
    Database.User.DB.rollback db
  )
  |> handle_error

let can_create_user () =
  let username = "John"
  and password = "password123"
  and email = "john@is-not-a-real-person.com" in
  database_test (fun db ->
    let+ result = 
      Database.User.DB.create_user db
        ~username ~password ~email in
    Alcotest.(check bool) "something is returned"
      true (Option.is_some result);
    (Option.iter (function
         Database.User.{username=u;email=e;password_hash=p;id} ->
         Alcotest.(check int) "id is 1" id 1;
         Alcotest.(check string) "username matches" username u;
         Alcotest.(check string) "email matches" email e;
         Alcotest.(check bool) "password does not matches"
           false (String.equal password p);
     ) result)
  )

let can_create_and_lookup_user () =
  let username = "John"
  and password = "password123"
  and email = "john@is-not-a-real-person.com" in
  database_test (fun db ->
    let* _ = 
      Database.User.DB.create_user db
        ~username ~password ~email in
    let+ result = Database.User.DB.find_user db ~username in
    Alcotest.(check bool) "something is returned"
      true (Option.is_some result);
    (Option.iter (function
         Database.User.{username=u;email=e;password_hash=p;id} ->
         Alcotest.(check int) "id is 1" id 1;
         Alcotest.(check string) "username matches" username u;
         Alcotest.(check string) "email matches" email e;
         Alcotest.(check bool) "password does not matches"
           false (String.equal password p);
     ) result)    
  )

let can_create_and_login_user () =
  let username = "John"
  and password = "password123"
  and email = "john@is-not-a-real-person.com" in
  database_test (fun db ->
    let* _ = 
      Database.User.DB.create_user db
        ~username ~password ~email in
    let* result = Database.User.DB.login db ~username ~password in
    Alcotest.(check bool) "logs in correctly"
      true (Option.is_some result);

    Lwt_result.return ()
  )


let can_migrate_posts_database () =
  with_db_connection (fun db ->
    let* () = User.DB.migrate db in
    let+ _ = Post.DB.migrate db in
    ()
  )
  |> handle_error

let can_rollback_posts_database () =
  with_db_connection (fun db ->
    Post.DB.rollback db
  )
  |> handle_error

let () =
  Lwt_main.run @@ Alcotest_lwt.run "test user" [
    "user database", [
      "can migrate", `Quick, can_migrate_user_database;
      "can rollback", `Quick, can_rollback_user_database;
      "can create user", `Quick, can_create_user;
      "can create and lookup user", `Quick, can_create_and_lookup_user;
      "can create and login user", `Quick, can_create_and_login_user
    ];
    "posts database", [
      "can migrate", `Quick, can_migrate_posts_database;
      "can rollback", `Quick, can_rollback_posts_database;

    ]
  ]
