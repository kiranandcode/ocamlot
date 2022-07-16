open Ppxlib
open Parsetree

let check_eq ~expected ~actual name =
  let expr = Alcotest.of_pp Ppxlib.Pprintast.expression in
  Alcotest.(check expr) name expected actual

let v =
  [%str type t = Epic]

let () =


  Alcotest.run "ppx_activitypub_test" [
    "builds"
  ]
