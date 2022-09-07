[@@@warning "-33-32"]
open Containers
open Common

module Runner = Runner

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


(* let handle_get_home config req =
 *   Common.with_current_user req @@ fun user ->
 *   with_current_time req @@ fun time offset ->
 * 
 *   let+ errors = Common.Error.get req in
 *   let errors = errors |> Option.map (Fun.flip List.cons []) |> Option.value ~default:[] in
 * 
 *   let+ posts =
 *     match user with
 *     | None ->
 *       Dream.sql req @@ (Database.Post.collect_post_whole_known_network ~offset:(time, 10, 10 * offset))
 *     | Some user ->
 *       let+! user =
 *         Dream.sql req (Database.Actor.of_local (Database.LocalUser.self user)) in
 *       Dream.sql req @@
 *       Database.Post.collect_post_feed
 *         ~offset:(time, 10, 10 * offset) user in
 *   let posts = posts |> Result.get_or ~default:[] in
 *   let+ posts =
 *     Dream.sql req @@ fun db ->
 *     Lwt_list.map_s (fun post ->
 *       let author = Database.Post.author post in
 *       let+! author = Database.Link.resolve author db in
 *       Lwt_result.return (author, post)
 *     ) posts in
 * 
 *   let posts = Result.flatten_l posts |> Result.get_or ~default:[] in
 *   
 *   Dream.html (Html.Home.build config ~offset:(time, offset) ~errors ~posts user req) *)


(* let handle_post_home config =
 *   let user_tag = Configuration.Regex.user_tag config |> Re.compile in
 *   let user_tag str =
 *     match Re.exec_opt user_tag str with
 *     | None -> None
 *     | Some matches -> Some (Re.Group.get matches 1, Re.Group.get matches 2) in
 *   fun req ->
 *     let+ form_results = Dream.form req in
 *     let> form_data = form_results
 *                      |> form_ok_or_else ~req "Invalid form submission: " in
 * 
 *     let lookup keyword = List.assoc_opt ~eq:String.equal keyword form_data in
 *     match lookup "follow", lookup "post" with
 *     | Some follow, _ ->
 *       Dream.log "following %s" follow;
 *       let> user = Common.with_current_user req in
 *       begin match user_tag follow with
 *       | None -> Dream.log "did not match anything!"
 *       | Some (username, domain) ->
 *         Option.iter (fun current_user ->
 *           Worker.(send req @@ LocalFollow {local=current_user; username; domain}))
 *           user;
 *         Dream.log "follow %s at %s" username domain
 *       end;
 *       (\*  run configuration.parse account *\)
 *       handle_get_home config req
 *     | _, Some post ->
 *       let> user = Common.with_current_user req in
 *       begin
 *         Option.iter (fun current_user ->
 *           Worker.(send req @@ LocalPost {user=current_user; content=post}))
 *           user;
 *       end;
 *       Dream.log "toasting %s" post;
 *       handle_get_home config req
 *     | _ ->
 *       Dream.log "form data: %s" @@ [%show: (string * string) list] form_data;
 *       handle_get_home config req *)

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

let (let+) x f = Lwt_result.bind x f

let run config =
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

  List.iter (fun p ->
    Dream.info (fun f -> f "found file %s" (Fpath.to_string p))
  ) (Bos.OS.Dir.contents (Fpath.of_string "/certs/" |> Result.get_exn) |> Result.get_exn);

  Option.iter (fun fl ->
    Dream.info (fun f -> f "cert file is %s\ncontents: %s" fl (IO.with_in fl IO.read_all));
  ) (Configuration.Params.certificate_file config);

  Option.iter (fun fl ->
    Dream.info (fun f -> f "key file is %s\ncontents: %s" fl (IO.with_in fl IO.read_all))
  ) (Configuration.Params.key_file config);


  Runner.run
    ~workers:[worker]
    ~tls:(Configuration.Params.is_tls_enabled config)
    ?certificate_file:(Configuration.Params.certificate_file config)
    ?key_file:(Configuration.Params.key_file config)
    ~port:(Configuration.Params.port config)
  @@ Dream.logger
  @@ Dream.sql_pool Configuration.Params.(database_path config)
  @@ Dream.sql_sessions
  @@ Dream.router [
    Webfinger.route config;

    Authentication.route config;
    Feed.route config;
    Write_post.route config;
    (* Authentication.route;
     * Actor.route config;
     * Activity.route config; *)

    (* Dream.get "/home" @@ (handle_get_home config);
     * Dream.post "/home" @@ (handle_post_home config); *)

    Dream.get "/static/**" @@ Dream.static ~loader:from_static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/feed"
  ]


