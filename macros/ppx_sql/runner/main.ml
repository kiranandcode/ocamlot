open Containers

let file = "../../../resources/schema.sql"
let schema = IO.with_in file IO.read_all |> Sql.parse |> Result.get_exn |> Sql.extract |> Result.get_exn

let file = "../../test/example_queries.sql"

let queries = IO.with_in file IO.read_all
              |> String.split ~by:";;\n"

let () =
  List.iteri (fun i s ->
    match Sql.Query.parse s with
    | Ok query ->
      begin match Sql.Query.infer schema query with
      | ty -> 
        Format.printf "[%d]: %a\n%s@." i Sql.Query.Type.pp_ty ty s
      | exception e ->
        Format.printf "[%d]: exn %s\n" i  (Printexc.to_string e);
        Printexc.print_backtrace stdout;
        Format.printf "[%d]: %s\n" i s;
      end
    | Error e ->
      Format.printf "[%d]: failed to parse because %s\n%s" i e s
    | exception e ->
      Format.printf "[%d]: exn %s@." i  (Printexc.to_string e);
      Printexc.print_backtrace stdout;
  ) queries
