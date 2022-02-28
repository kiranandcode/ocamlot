let tests = ref []

let run name =
  Alcotest.run name
    (List.map (fun f -> f ()) @@ List.rev !tests)

module Make (S: sig val name: string end) = struct

  let module_tests = ref [];;

  tests := (fun () -> S.name, List.rev !module_tests) :: !tests

  let add_test name test =
    module_tests := (name, `Quick, test) :: !module_tests

  let add_slow_test name test =
    module_tests := (name, `Slow, test) :: !module_tests

  let run () = run S.name

end

module Lwt = struct
  let tests = ref []


  let run name =
    Lwt_main.run @@
    Alcotest_lwt.run name
      (List.map (fun f -> f ()) @@ List.rev !tests)


  module Make (S: sig val name : string end) = struct

    let module_tests = ref [];;

    tests := (fun () -> S.name, List.rev !module_tests) :: !tests;;

    let add_test name test =
      module_tests := (name, `Quick, test) :: !module_tests

    let add_slow_test name test =
      module_tests := (name, `Slow, test) :: !module_tests

    let run () = run S.name
  end

end
