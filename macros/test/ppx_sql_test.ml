open Containers

let (let-!) x f = match x with Ok x -> f x | _ -> ()
let schema = IO.with_in "../../resources/schema.sql" IO.read_all
let queries = IO.with_in "./example_queries.sql" IO.read_all |> String.split ~by:";;\n"

let can_parse_schema () =
  let schema = schema |> Sql.parse in
  Alcotest.(check (bool)) "schema can be parsed"
    (Result.is_ok schema)
    true

let can_extract_schema () =
  let-! schema = schema |> Sql.parse in
  let result = Sql.extract schema in
  Alcotest.(check (bool)) "schema can be extracted"
    (Result.is_ok result)
    true

let pp_result : Format.formatter -> ('a, string) result -> unit =
  fun fmt res ->
  match res with
  | Ok _ -> Format.pp_print_string fmt "Ok(_)"
  | Error msg -> Format.fprintf fmt "Error(\"%s\")" msg
  
let is_ok_test () = Alcotest.testable (Option.pp pp_result) (fun v1 v2 ->
  match v1, v2 with
  | None, None -> true
  | None, Some (Ok _) | Some (Ok _), None -> true
  | None, Some (Error _) | Some (Error _), None -> false
  | Some (Error m1), Some (Error m2) -> String.equal m1 m2
  | Some (Ok _), Some (Ok _) -> true
  | Some _, Some _ -> false
)

let query_parsing_tests  =
  let is_ok = is_ok_test () in
  List.mapi (fun i txt ->
    let name = Format.sprintf "can parse query %d" i in
    let test_name = Format.sprintf "query %d parses without error" i in
    let test () =
      Alcotest.(check is_ok) test_name
      (Some (Sql.Query.parse txt))
      None in
    name, `Quick, test
  ) queries

let query_inference_tests  =
  let is_ok = is_ok_test () in
  List.mapi (fun i txt ->
    let name = Format.sprintf "can infer types of query %d" i in
    let test_name = Format.sprintf "query %d infers without error" i in
    let test () =
      let-! schema = schema |> Sql.parse in
      let-! schema = Sql.extract schema in
      let-! query = (Sql.Query.parse txt) in
      Alcotest.(check is_ok) test_name
      (Some (Sql.Query.infer schema query))
      None in
    name, `Quick, test
  ) queries

  

let () =
  Alcotest.run "ppx_sql" [
    "schema", [
      "can be parsed", `Quick, can_parse_schema;
      "can be extracted", `Quick, can_extract_schema
    ];
    "query", (query_parsing_tests @ query_inference_tests)
  ]



