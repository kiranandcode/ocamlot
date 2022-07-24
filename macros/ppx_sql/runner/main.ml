open Containers

let file = "../../test/example_queries.sql"

let queries = IO.with_in file IO.read_all
              |> String.split ~by:";;\n"

let () =
  Utils.should_log := false;
  List.iteri (fun i s ->
    match Parser.parse_query s with
    | Ok _query -> ()
      (* Format.printf "[%d]: %a\n" i Ast.pp_query query *)
    | Error e ->
      Utils.should_log := true;
      let _ = Parser.parse_query s in
      Utils.should_log := false;
      Format.printf "[%d]: failed to parse because %s\n%s" i e s
    | exception e ->
      Format.printf "[%d]: exn %s" i  (Printexc.to_string e);
      Printexc.print_backtrace stdout;
  ) queries
