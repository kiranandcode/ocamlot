module Common = struct
  open Bos
  open Containers
  module Ty = Caqti_type.Std
  module type DB = Caqti_lwt.CONNECTION
  let (let+) x f = Lwt.bind x f
  let (let@) x f = Lwt.(x >|= f)
  let (let*) x f = Lwt.(x >|= function v -> f (Result.get_exn v))
  let ret = Lwt.return_unit


  let check_is_true vl = Alcotest.(check bool) "predicate is true" true vl
  let check_is_false vl = Alcotest.(check bool) "predicate is false" false vl
  let check_is_ok user = Alcotest.(check bool) "result is ok" true (Result.is_ok user)
  let check_is_some vl = Alcotest.(check bool) "result is something" true (Option.is_some vl)
  let check_string_eq ~expected value = Alcotest.(check string) "string matches" expected value
  let check_string_neq ~expected value = Alcotest.(check bool) "string does not match" false String.(expected = value)

  let with_db f () =
    let db_name = "/tmp/ocamlot_test" ^ Int.to_string (Stdlib.Random.int 20_000) ^ ".db" in
    let test_db_path = Fpath.of_string db_name |> Result.get_exn in
    let create_db_cmd = Cmd.(v "sqlite3" % "-init" % "../resources/schema.sql" % db_name) in

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
    print_endline ("database: " ^ db_name);
    f (module DB : Caqti_lwt.CONNECTION)

end



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


