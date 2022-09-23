[@@@warning "-32"]
module Common = struct
  open Containers
  module Ty = Caqti_type.Std
  module type DB = Caqti_lwt.CONNECTION
  let (let+) x f = Lwt.bind x f
  (* let (let+!) x f = Lwt_result.bind x f
   * let (let@) x f = Lwt.(x >|= f) *)
  let (let*) x f = Lwt.(x >>= function v -> f (Result.get_exn v))
  let ret v = Lwt.return v

  let (>>=) x f = Lwt_result.bind x f
  let (>|=) x f = Lwt_result.map f x


  let check_is_true vl = Alcotest.(check bool) "predicate is true" true vl
  let check_is_false vl = Alcotest.(check bool) "predicate is false" false vl
  let check_is_ok user = Alcotest.(check bool) "result is ok" true (Result.is_ok user)
  let check_is_some vl = Alcotest.(check bool) "result is something" true (Option.is_some vl)
  let check_int_eq ~expected value = Alcotest.(check int) "string matches" expected value
  let check_string_eq ~expected value = Alcotest.(check string) "string matches" expected value
  let check_string_neq ~expected value = Alcotest.(check bool) "string does not match" false String.(expected = value)


  let init_requests =
    let init_statements = IO.with_in "../resources/schema.sql" IO.read_all
                          |> String.split_on_char ';'
                          |> List.map String.trim
                          |> List.filter (Fun.negate String.is_empty) in
    let create_opaque_request schema =
      let schema _ = Caqti_query.of_string_exn schema in
      Caqti_request.Infix.(
        Caqti_type.unit -->. Caqti_type.unit
      ) ~oneshot:true schema in
    List.map create_opaque_request init_statements

  let with_db (f: (module Caqti_lwt.CONNECTION) -> 'a Lwt.t) () =
    let+ connection = Caqti_lwt.connect (Uri.make ~scheme:"sqlite3" ~path:":memory:" ()) in
    let connection = Result.get_exn connection in
    let (module DB) = connection in
    let exec req =
      let+ result = DB.exec req () in
      let () = result
               |> Result.map_err Caqti_error.show
               |> Result.get_or_failwith in
      Lwt.return () in

    let+ () = Lwt_list.iter_s exec init_requests  in
    let+ res = f (module DB : Caqti_lwt.CONNECTION) in
    let+ () = DB.disconnect () in
    Lwt.return res

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


