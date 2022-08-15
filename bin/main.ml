[@@@warning "-33"]
open Containers
open Bos
open Cmdliner
let ( let* ) x f = Result.( let* ) x f
let map_e f = Result.map_err (fun (`Msg e) -> `Msg (f e))
let sql_err = function Sqlite3.Rc.OK -> Ok () | err -> Error (`Msg (Sqlite3.Rc.to_string err))

let default_schema = [%blob "../resources/schema.sql"]

let init_database path =
  let db = Sqlite3.db_open path in
  let* () = Sqlite3.exec db default_schema |> sql_err in
  let close_count = ref 0 in
  let close_status = ref (Sqlite3.db_close db) in
  while not !close_status && !close_count < 10 do
    incr close_count;
    close_status := Sqlite3.db_close db; 
  done;
  if not !close_status
  then Error (`Msg ("failed to close database"))
  else Ok (())

(** [enforce_database path] when given a path [path] ensures that
    database exists at [path], creating it if not. *)
let enforce_database path =
  let* path = Fpath.of_string path in
  let* exists = OS.File.exists path in
  if exists
  then Ok (Fpath.to_string path)
  else begin
    let* () =
      try
        init_database (Fpath.to_string path)
      with Sqlite3.SqliteError err -> Error (`Msg ("failed to initialise a fresh database: " ^ err)) in
    Ok (Fpath.to_string path)
  end

let run database_path domain port =
  let* database_path = enforce_database database_path in
  let database_path =  "sqlite3://:" ^ database_path in
  let config = Configuration.Params.create ?port ~database_path domain in
  Ok (Server.run config)

let database_path =
  let info =
    Arg.info
      ~doc:{| $(docv) is the path to the database used by OCamlot. The database is generated if not present.  |}
      ~docv:"DB"
      ~absent:{|A fresh database is generated in the current working directory.|}
      ["f"; "database-path"] in
  Arg.(opt file "./ocamlot.db") info

let domain =
  let info =
    Arg.info
      ~doc:{| $(docv) is the domain on which the server is running. This is important for verifying the signatures of incoming messages, as they will expect an appropriate domain in the header.  |}
      ~docv:"DOMAIN"
      ~absent:{|The domain defaults to localhost.|}
      ["d"; "domain"] in
  Arg.(opt string "localhost") info

let port =
  let info =
    Arg.info
      ~doc:{| $(docv) is the port on which the OCamlot server should run. |}
      ~docv:"PORT"
      ~absent:{|The port defaults to 7331.|}
      ["p"; "port"] in
  Arg.(opt (some int) (Some 7331)) info

let _ =
  let info =
    Cmdliner.Cmd.info
      ~version:{|%%VERSION%%|}
      ~doc:"An OCaml Activitypub Server *with soul*!"
      "OCamlot" in
  let cmd = Term.(term_result @@ (const run $ Arg.value database_path $ Arg.value domain $ Arg.value port)) in
  Cmd.eval (Cmd.v info cmd)
