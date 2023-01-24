[@@@warning "-33-32"]
open Containers
open Common

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

let from_static local_root path req =
  let mime_lookup filename =
    let content_type =
      match Magic_mime.lookup filename with
      | "text/html" -> Dream_pure.Formats.text_html
      | content_type -> content_type
    in
    ["Content-Type", content_type] in
  Dream.debug (fun f -> f ~request:req "request static %s %s" local_root path);
  match Static.read path with
  | Some contents ->
    Dream.respond
      ~headers:(mime_lookup path)
      contents
  | None ->
    Dream.respond ~status:`Not_Found ""

let enforce_domain config: Dream.middleware =
  (fun f -> (fun req ->
     Dream.set_header req "host" (Configuration.Params.host config);
     f req))

let (let+) x f = Lwt_result.bind x f

let run config =
  let () = Mirage_crypto_rng_lwt.initialize () in
  let task_in, send_task = Lwt_stream.create () in
  if Configuration.Params.debug config then begin
    Dream.initialize_log ~level:`Info ~enable:true ();
    Dream.info (fun f -> f "Running OCamlot in debugging mode.");
    Logging.set_log_level `Debug;
  end;

  Configuration.Params.set_task_fn config send_task;

  let worker = Worker.init config task_in |> Lwt.map ignore in

  if Configuration.Params.is_tls_enabled config then
    Dream.info (fun f -> f "Enabled HTTPS/TLS for OCamlot server");

  Option.iter (fun fl ->
    Dream.info (fun f -> f "cert file is %s\ncontents: %s" fl (IO.with_in fl IO.read_all));
  ) (Configuration.Params.certificate_file config);

  Option.iter (fun fl ->
    Dream.info (fun f -> f "key file is %s\ncontents: %s" fl (IO.with_in fl IO.read_all))
  ) (Configuration.Params.key_file config);


  Dream_runner.run
    ~workers:[worker]
    ~tls:(Configuration.Params.is_tls_enabled config)
    ?certificate_file:(Configuration.Params.certificate_file config)
    ?key_file:(Configuration.Params.key_file config)
    ~port:(Configuration.Params.port config)
  @@ Dream.logger
  @@ Dream.sql_pool Configuration.Params.(database_path config)
  @@ Dream.sql_sessions
  @@ enforce_domain config
  @@ Dream.router [
    Webfinger.route config;

    Authentication.route config;
    Feed.route config;
    Users.route config;
    Write_post.route config;
    Activity.route config;

    Images.route config;
    (* Dream.get "/home" @@ (handle_get_home config);
     * Dream.post "/home" @@ (handle_post_home config); *)

    Dream.get "/static/**" @@ Dream.static ~loader:from_static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/feed"
  ]


