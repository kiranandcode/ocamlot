open Lwt.Syntax

let can_create_user_database () =

  Lwt.return ()

let () =
  Lwt_main.run @@ Alcotest_lwt.run "test user" [
    "user database", [
      "can create", `Quick, can_create_user_database
    ]
  ]
