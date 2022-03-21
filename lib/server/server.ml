[@@@warning "-33"]
open Containers
open Common

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


let handle_get_home config req =
  Common.with_current_user req @@ fun user ->
  with_current_time req @@ fun time offset ->

  let+ errors = Common.Error.get req in
  let errors = errors |> Option.map (Fun.flip List.cons []) |> Option.value ~default:[] in

  let+ posts =
    match user with
    | None ->
      Dream.sql req @@ (Database.Post.collect_post_whole_known_network ~offset:(time, 10, 10 * offset))
    | Some user ->
      let+! user =
        Dream.sql req (Database.Actor.of_local (Database.LocalUser.self user)) in
      Dream.sql req @@
      Database.Post.collect_post_feed
        ~offset:(time, 10, 10 * offset) user in
  let posts = posts |> Result.get_or ~default:[] in
  let+ posts =
    Dream.sql req @@ fun db ->
    Lwt_list.map_s (fun post ->
      let author = Database.Post.author post in
      let+! author = Database.Link.resolve author db in
      Lwt_result.return (author, post)
    ) posts in

  let posts = Result.flatten_l posts |> Result.get_or ~default:[] in
  
  Dream.html (Html.Home.build config ~offset:(time, offset) ~errors ~posts user req)


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
          Worker.(send req @@ Follow {local=current_user; username; domain}))
          user;
        Dream.log "follow %s at %s" username domain
      end;
      (*  run configuration.parse account *)
      handle_get_home config req
    | _, Some post ->
      let> user = Common.with_current_user req in
      begin
        Option.iter (fun current_user ->
          Worker.(send req @@ Post {user=current_user; content=post}))
          user;
      end;
      Dream.log "toasting %s" post;
      handle_get_home config req
    | _ ->
      Dream.log "form data: %s" @@ [%show: (string * string) list] form_data;
      handle_get_home config req

let caqti path = Caqti_lwt.connect (Uri.of_string path)
                 |> Lwt.map Result.get_exn


let () =
  let database_path =  "sqlite3://:../../test.db" in
  let config =
    Configuration.Params.create
      ~database_path ~domain:(* "ocamlot.nfshost.com" *)(* "localhost:4000" *)
      "testing.ocamlot.xyz"
  in
  Worker.init config;
  Dream.run
    ~certificate_file:"/etc/letsencrypt/live/testing.ocamlot.xyz/fullchain.pem"
    ~key_file:"/etc/letsencrypt/live/testing.ocamlot.xyz/privkey.pem"
    ~port:443
  @@ Dream.logger
  @@ Dream.sql_pool database_path
  @@ Dream.sql_sessions
  @@ Dream.router [
    Webfinger.route config;

    Authentication.route;

    Actor.route config;

    Activity.route config;

    Dream.get "/home" @@ (handle_get_home config);
    Dream.post "/home" @@ (handle_post_home config);

    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/home"
  ]
