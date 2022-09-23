module T = Testing_utils.Make (struct let name = "password" end) ;;

let check_is_ok fn () =
  Alcotest.(check bool) "result is ok"
    true
    (Result.is_ok (fn ()));;

let check_is_ok_true fn () =
  Alcotest.(check (result bool string)) "result is ok"
    (Ok true)
    (fn ());;



T.add_test "hashing password doesn't fail" @@ check_is_ok (fun () -> Password.hash ~pwd:"password123") ;;

T.add_test "hashed password can verify actual password" @@ check_is_ok_true (fun () ->
  let hash = Password.hash ~pwd:"password123" |> Result.get_ok in
  Password.verify hash ~pwd:"password123"
  ) ;;


let () = T.run ()
