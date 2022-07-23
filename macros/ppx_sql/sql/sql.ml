open Containers
module Types = Types
module Builder = Builder

let parse s = Angstrom.(parse_string ~consume:Consume.All Parser.program s)
let extract ?(loc=Location.none) s = Result.guard_str (fun () -> Extract.process loc s)
