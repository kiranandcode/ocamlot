
let () = declare_schema "../../resources/schema.sql"



type%sql.generate random = SQL [@schema "RemoteUser"]

let () =
  print_endline "random"

