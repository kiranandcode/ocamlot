[@@@warning "-33"]
open Containers
open Common

module Worker = struct
  let worker_var = Lwt_mvar.create_empty ()

  let send config ~current_user ~username ~domain  =
    Lwt.async (fun () -> Lwt_mvar.put worker_var (config, current_user, username, domain))

  let rec worker db =
    let+ (config, local, username, domain) = Lwt_mvar.take worker_var in
    let+ res = Resolver.follow_remote_user config local ~username ~domain db in
    match res with
    | Ok () -> worker db
    | Error e ->
      Dream.error (fun log -> log "error in worker: %s" e);
      worker db

  let init config =
    Lwt.async @@ fun () -> 
    let+ db =
      config
      |> Configuration.Params.database_path
      |> Uri.of_string
      |> Caqti_lwt.connect in
    let db = Result.get_exn db in
    worker db

end  

let failwith ~req err = let+ () = Common.Error.set req err in Dream.redirect req "/home"
let holds_or_else ~req red vl kont = match vl with false -> failwith ~req red | true -> kont ()
let ok_or_else ~req red vl kont = match vl with Error e -> failwith ~req (red ^ e) | Ok v -> kont v
let or_else ~req red vl kont = match vl with None -> failwith ~req red | Some v -> kont v
let form_ok_or_else ~req red (vl: _ Dream.form_result) kont =
  match vl with
  | `Many_tokens _ -> failwith ~req (red ^ "many tokens")
  | `Missing_token _ -> failwith ~req (red ^ "missing tokens")
  | `Invalid_token _ -> failwith ~req (red ^ "invalid token")
  | `Wrong_session _ -> failwith ~req (red ^ "wrong session")
  | `Expired _ -> failwith ~req (red ^ "expired form")
  | `Wrong_content_type -> failwith ~req (red ^ "wrong content type")
  | `Ok v -> kont v

let handle_get_home req =
  Common.with_current_user req @@ fun user ->
  let+ errors = Common.Error.get req in
  let errors = errors |> Option.map (Fun.flip List.cons []) |> Option.value ~default:[] in
  Dream.html (Html.Home.build ~errors user req)


let handle_post_home config =
  let user_tag = Configuration.Regex.user_tag config |> Re.compile in
  let user_tag str =
    match Re.exec_opt user_tag str with
    | None -> None
    | Some matches -> Some (Re.Group.get matches 1, Re.Group.get matches 2) in
  fun req ->
    let+ form_results = Dream.form req in
    let> form_data = form_results
                     |> form_ok_or_else ~req "Invalid form submission: " in

    let lookup keyword = List.assoc_opt ~eq:String.equal keyword form_data in
    match lookup "follow", lookup "post" with
    | Some follow, _ ->
      Dream.log "following %s" follow;
      let> user = Common.with_current_user req in
      begin match user_tag follow with
      | None -> Dream.log "did not match anything!"
      | Some (username, domain) ->
        Option.iter (fun current_user ->
          Worker.send config ~current_user ~username ~domain) user;
        Dream.log "follow %s at %s" username domain
      end;
      (*  run configuration.parse account *)
      handle_get_home req
    | _, Some post ->
      Dream.log "toasting %s" post;
      handle_get_home req
    | _ ->
      Dream.log "form data: %s" @@ [%show: (string * string) list] form_data;
      handle_get_home req

let caqti path = Caqti_lwt.connect (Uri.of_string path)
                 |> Lwt.map Result.get_exn


let () =
  let database_path =  "sqlite3://:../../test.db" in
  let config =
    Configuration.Params.create
      ~database_path ~domain:"ocamlot.nfshost.com" in
  Worker.init config;
  Dream.run
    (* ~certificate_file:"/home/kirang/Documents/code/elixir/pleroma/priv/server.pem"
     * ~key_file:"/home/kirang/Documents/code/elixir/pleroma/priv/server.key" *)
    ~port:4000
  @@ Dream.logger
  @@ Dream.sql_pool database_path
  @@ Dream.sql_sessions 
  @@ Dream.router [
    Webfinger.route config;

    Authentication.route;

    Actor.route config;

    Activity.route config;

    Dream.get "/home" @@ handle_get_home;
    Dream.post "/home" @@ (handle_post_home config);

    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/home"
  ]
