
let () = declare_schema "../../resources/schema.sql"



type%sql.check[@schema "Activity"] random = {
  data: int
}

let () =
  print_endline "hello"

