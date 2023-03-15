[@@@warning "-33-32"]
open Containers
open Common

let log = Logging.add_logger "server"

module Runner = Dream_runner

let with_current_time req f =
  let time = Dream.query req "time"
             |> Fun.flip Option.bind (fun v ->  Ptime.of_rfc3339 v |> Result.to_opt)
             |> Option.map (fun (time, _, _) -> time)
             |> Option.value ~default:(Ptime_clock.now ()) in
  let offset = Dream.query req "offset"
               |> Fun.flip Option.bind Int.of_string
               |> Option.value ~default:0 in
  let time = time
             |> Ptime.to_float_s
             |> CalendarLib.Calendar.from_unixfloat in
  f time offset

let caqti path = Caqti_lwt.connect (Uri.of_string path)
                 |> Lwt.map Result.get_exn

let from_static _local_root path _req =
  let mime_lookup filename =
    let content_type =
      match Magic_mime.lookup filename with
      | "text/html" -> Dream_pure.Formats.text_html
      | content_type -> content_type
    in
    ["Content-Type", content_type] in
  match Static.read path with
  | Some contents ->
    Dream.respond
      ~headers:(mime_lookup path)
      contents
  | None ->
    Dream.respond ~status:`Not_Found ""

let enforce_domain: Dream.middleware =
  (fun f req -> 
     Dream.set_header req "host" (Lazy.force Configuration.host);
     f req)


let run () =
  let () = Mirage_crypto_rng_lwt.initialize () in
  if Lazy.force Configuration.debug then begin
    if Lazy.force Configuration.debug_dream_info
    then Dream.initialize_log ~level:`Info ~enable:true ()
    else (
      Dream.initialize_log ~level:`Error ~enable:true ();
      List.iter (fun src ->
        Logs.Src.set_level src (Some Logs.Error);
      ) (Logs.Src.list ());
    );

    Dream.info (fun f -> f "Running OCamlot in debugging mode.");

    Logging.set_log_level `Debug;

  end;


  let worker = Worker.init (Lazy.force Configuration.database_uri) |> Lwt.map ignore in

  if Lazy.force Configuration.is_tls_enabled then
    Dream.info (fun f -> f "Enabled HTTPS/TLS for OCamlot server");

  Option.iter (fun fl ->
    Dream.info (fun f -> f "cert file is %s\ncontents: %s" fl (IO.with_in fl IO.read_all));
  ) (Lazy.force Configuration.certificate_file);

  Option.iter (fun fl ->
    Dream.info (fun f -> f "key file is %s\ncontents: %s" fl (IO.with_in fl IO.read_all))
  ) (Lazy.force Configuration.key_file);

  Dream_runner.run
    ~workers:[worker]
    ~tls:(Lazy.force Configuration.is_tls_enabled)
    ?certificate_file:(Lazy.force Configuration.certificate_file)
    ?key_file:(Lazy.force Configuration.key_file)
    ~port:(Lazy.force Configuration.port)
  @@ Dream.logger
  @@ Dream.sql_pool (Lazy.force Configuration.(database_uri))
  @@ Dream.sql_sessions
  @@ enforce_domain
  @@ Dream.router [
    Webfinger.route;

    Authentication.route;
    Feed.route;
    Users.route;
    Write_post.route;
    Activity.route;
    Post.route;
    Follow_requests.route;
    Settings.route;
    Images.route;

    (* Dream.get "/home" @@ (handle_get_home config); *)
    (* Dream.post "/home" @@ (handle_post_home config); *)


    Dream.get "/static/**" @@ Dream.static ~loader:from_static "static";
    Dream.get "/**" @@ fun req -> (
      log.debug (fun f -> f "received request target:%s" (Dream.target req));
      Dream.redirect req "/feed"
    )
  ]


