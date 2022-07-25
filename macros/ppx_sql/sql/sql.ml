open Containers
module Types = Types
module Builder = Builder

module Query = struct
  module Ast = Query_ast
  module Type = Query_type
  module Builder = Query_builder
  let parse s = fst @@ Utils.with_fresh_ids (fun () -> Utils.parse_safe Query_parser.query s)
  let infer schema q = Result.guard_str_trace @@ fun () -> Query_inference.type_of_query schema q
end

let parse s = Utils.parse_safe Parser.program s
let extract ?(loc=Location.none) s = Result.guard_str (fun () -> Extract.process loc s)


