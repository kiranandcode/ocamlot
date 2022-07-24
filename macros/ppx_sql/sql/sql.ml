open Containers
module Types = Types
module Builder = Builder


let parse s = Utils.parse_safe Parser.program s
let extract ?(loc=Location.none) s = Result.guard_str (fun () -> Extract.process loc s)

let parse_query s = Utils.parse_safe Query_parser.query s
