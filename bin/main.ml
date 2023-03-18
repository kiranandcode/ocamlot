[@@@warning "-33-32"]
open Containers
open Bos
open Cmdliner
let ( let* ) x f = Result.( let* ) x f
let map_e f = Result.map_err (fun (`Msg e) -> `Msg (f e))
let sql_err = function Sqlite3.Rc.OK -> Ok () | err -> Error (`Msg (Sqlite3.Rc.to_string err))

let init_database ?(force_migrations=false) path =
  let initialise =
    let open Lwt_result.Syntax in
    Caqti_lwt.with_connection path (fun conn ->
      let* needs_migration =
        Petrol.VersionedSchema.migrations_needed Database.Tables.db conn in
      Format.printf "needs migration: %b\n@." needs_migration;
      let* () =
        if needs_migration && not force_migrations
        then
          Lwt_result.fail
            (`Msg "migrations needed for local database - please \
                   re-run with suitable flags (-m). (You probably also \
                   want to backup your database file as well)")
        else Lwt_result.return () in
      Petrol.VersionedSchema.initialise Database.Tables.db conn
    ) in
  let version_to_string (ver: Petrol.VersionedSchema.version) =
    String.concat "." (List.map string_of_int (ver :> int list)) in
  match Lwt_main.run initialise with
  | Ok () -> Ok ()
  | Error (`Newer_version_than_supported version) ->
    Error (`Msg
             (
               "database uses newer version (" ^
               (version_to_string version) ^
               ") than supported"))
  | Error (#Caqti_error.t as err) ->
    Error (`Msg ("internal error: " ^ Caqti_error.show err))
  | Error (`Msg m) -> Error (`Msg m)


(** [enforce_directory path] when given a path [path] ensures that
    a directory exists at [path], creating it if not. *)
let enforce_directory path =
  let* path = Fpath.of_string path in
  let* exists = OS.Dir.exists path in
  if exists
  then Ok (Fpath.to_string @@Fpath.to_dir_path path)
  else begin
    let* _ = OS.Dir.create path in
    Ok (Fpath.to_string @@ Fpath.to_dir_path path)
  end

(** [enforce_database path] when given a path [path] ensures that
    database exists at [path], creating it if not. *)
let enforce_sqlite_database ?force_migrations path =
  let* path = Fpath.of_string path in
  (* we call exists here just to make sure that it is a file if it exists and not a directory *)
  let* _ = OS.File.exists path in
  let* () =
    try
      init_database ?force_migrations (Uri.of_string ("sqlite3://:" ^ (Fpath.to_string path)))
    with Sqlite3.SqliteError err -> Error (`Msg ("failed to initialise a fresh database: " ^ err)) in
  Ok ()

(** [enforce_postgres_database url] when given a [url] ensures that a
    postgres database exists at [path], creating it if not. *)
let enforce_postgres_database ?force_migrations url =
  let* () =
    try
      init_database ?force_migrations (Uri.of_string ("postgresql://" ^ url))
    with Postgresql.Error err -> Error (`Msg ("failed to initialise a fresh database: " ^
                                              Postgresql.string_of_error err)) in
  Ok ()


(** [enforce_markdown path] when given a path [path] ensures that
    a markdown file exists at [path]. *)
let enforce_markdown path =
  let* path = Fpath.of_string path in
  let* path = OS.File.must_exist path in
  let* contents = OS.File.read path in
  Ok (Omd.of_string contents)

let run promote_admin =

  let* () =
    match Lazy.force Configuration.database_dialect with
    | `Sqlite3 ->
      enforce_sqlite_database
        ~force_migrations:(Lazy.force Configuration.force_migrations)
        (Lazy.force Configuration.database_path)
    | `Postgres ->
      enforce_postgres_database
        ~force_migrations:(Lazy.force Configuration.force_migrations)
        (Option.get_exn_or "invalid assumptions -- postgres url not found"
            (Lazy.force Configuration.postgres_url)) in
  let database_url = Lazy.force Configuration.database_uri in

  let* _ =
    match Lazy.force Configuration.dump_json_dir with
    | None -> Ok ()
    | Some dir ->
      let* _ = enforce_directory dir in
      Ok () in

  let* _ =
    let* path = enforce_directory (Lazy.force Configuration.user_image_dir) in
    Ok path in

  match promote_admin with
  | Some username ->
    Lwt_main.run (Caqti_lwt.with_connection (Uri.of_string database_url) (fun db ->
      Database.LocalUser.promote_user_to_admin ~username db
    ) |> Lwt_result.map_error (fun err -> `Msg (Caqti_error.show err)))
  | None -> Ok (Server.run ())


open Cmdliner

let promote_admin =
  let info = Arg.info ~doc:"If provided, then rather than running the server, simply promote the requested user to an admin."
               ["promote-to-admin"] in
  Arg.value @@ Arg.opt Arg.(some string) None info


let _ =
  let info =
    Cmdliner.Cmd.info
      ~version:{|%%VERSION%%|}
      ~doc:"An OCaml Activitypub Server *with soul*!"
      "OCamlot" in
  let cmd = Term.(term_result @@ (
    Configuration.(wrap (const run)) $
    promote_admin)) in
  Cmd.eval (Cmd.v info cmd)
