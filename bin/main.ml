[@@@warning "-33"]
open Containers
open Bos
open Cmdliner
let ( let* ) x f = Result.( let* ) x f
let map_e f = Result.map_err (fun (`Msg e) -> `Msg (f e))
let sql_err = function Sqlite3.Rc.OK -> Ok () | err -> Error (`Msg (Sqlite3.Rc.to_string err))

let init_database ?(force_migrations=false) path =
  let initialise =
    let open Lwt_result.Syntax in
    Caqti_lwt.with_connection (Uri.of_string ("sqlite3://:" ^ path)) (fun conn ->
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
let enforce_database ?force_migrations path =
  let* path = Fpath.of_string path in
  (* we call exists here just to make sure that it is a file if it exists and not a directory *)
  let* _ = OS.File.exists path in
  let* () =
    try
      init_database ?force_migrations (Fpath.to_string path)
    with Sqlite3.SqliteError err -> Error (`Msg ("failed to initialise a fresh database: " ^ err)) in
  Ok (Fpath.to_string path)


(** [enforce_markdown path] when given a path [path] ensures that
    a markdown file exists at [path]. *)
let enforce_markdown path =
  let* path = Fpath.of_string path in
  let* path = OS.File.must_exist path in
  let* contents = OS.File.read path in
  Ok (Omd.of_string contents)

let run () =
  let* database_path = enforce_database
      ~force_migrations:(Configuration.Params.force_migrations ())
      (Configuration.Params.database_path ()) in
  (* let* about_this_instance = *)
  (*   match Configuration.Params.about_this_instance () with *)
  (*   | None -> Ok None *)
  (*   | Some path -> *)
  (*     let* path = Fpath.of_string path in *)
  (*     let* about_this_instance = OS.File.read path in *)
  (*     Ok (Some about_this_instance) in *)
  let database_path =  "sqlite3://:" ^ database_path in
  let* user_image_path =
    let* path = enforce_directory (Configuration.Params.user_image_path ()) in
    Ok path in

  Ok (Server.run config)



let _ =
  let info =
    Cmdliner.Cmd.info
      ~version:{|%%VERSION%%|}
      ~doc:"An OCaml Activitypub Server *with soul*!"
      "OCamlot" in
  let cmd = Term.(term_result @@ (
    Configuration.Params.wrap (const run) $
    const ())) in
  Cmd.eval (Cmd.v info cmd)
