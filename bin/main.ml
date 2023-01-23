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
        let* () =
          if needs_migration && not force_migrations
          then
            Lwt_result.fail
              (`Msg "migrations needed for local database - please re-run \
                     with suitable flags (-m).")
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
  then Ok (Fpath.to_string path)
  else begin
    let* _ = OS.Dir.create path in
    Ok (Fpath.to_string path)
  end

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

(** [enforce_markdown path] when given a path [path] ensures that
    a markdown file exists at [path]. *)
let enforce_markdown path =
  let* path = Fpath.of_string path in
  let* path = OS.File.must_exist path in
  let* contents = OS.File.read path in
  Ok (Omd.of_string contents)

let run key_file certificate_file user_image_path about_this_instance_path database_path domain port debug =
  let* database_path = enforce_database database_path in
  let* about_this_instance =
    match about_this_instance_path with
    | None -> Ok None
    | Some path ->
      let* path = Fpath.of_string path in
      let* about_this_instance = OS.File.read path in
      Ok (Some about_this_instance) in
  let database_path =  "sqlite3://:" ^ database_path in
  let* user_image_path = match user_image_path with
    | None -> Ok None
    | Some path ->
      let* path = enforce_directory path in
      Ok (Some path) in
  let config = Configuration.Params.create
                 ?key_file ?certificate_file ?user_image_path ?about_this_instance ~debug ?port ~database_path domain in
  Ok (Server.run config)

let debug =
  let info =
    Arg.info
      ~doc:{| Determines whether the OCamlot server should be run in debug mode. |}
      ["D"; "debug"] in
  Arg.flag info

let about_this_instance_path =
  let info =
    Arg.info
      ~doc:{| $(docv) is the path to a markdown file describing the instance. A default message is used if not provided.  |}
      ~docv:"ABOUT-THIS-INSTANCE"
      ~absent:{|A default message is used if not provided.|}
      ["a"; "about-this-instance"] in
  Arg.(opt (some file) None) info

let user_image_path =
  let info =
    Arg.info
      ~doc:{| $(docv) is the path to the directory used by OCamlot to store user images. The directory is created if not present.  |}
      ~docv:"DB"
      ~absent:{| A directory `user-images` in current working directory is used.|}
      ["u"; "user-image-path"] in
  Arg.(opt (some file) None) info

let key_file_path =
  let info =
    Arg.info
      ~doc:{| $(docv) is the path to the key file for the domain on which this server will be running.  |}
      ~docv:"KEY-FILE"
      ~absent:{|The server will not use TLS encryption (use a proxy like Nginx to enable ssl in that case).|}
      ["k"; "key-file"] in
  Arg.(opt (some file) None) info

let certificate_file_path =
  let info =
    Arg.info
      ~doc:{| $(docv) is the path to the certificate file for the domain on which this server will be running.  |}
      ~docv:"CERTIFICATE-FILE"
      ~absent:{|The server will not use TLS encryption (use a proxy like Nginx to enable ssl in that case).|}
      ["c"; "certificate-file"] in
  Arg.(opt (some file) None) info


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
  let cmd = Term.(term_result @@ (
      const run $
      Arg.value key_file_path $
      Arg.value certificate_file_path $
      Arg.value user_image_path $
      Arg.value about_this_instance_path $
      Arg.value database_path $
      Arg.value domain $
      Arg.value port $
      Arg.value debug)) in

  Cmd.eval (Cmd.v info cmd)
