[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "web.auth"

let check_unauthenticated = Common.Middleware.redirect_if_present "user" ~to_:"/feed"

let form_data key form = List.Assoc.get ~eq:String.equal key form |> Option.to_result (Format.sprintf "missing field %s" key)

let ensure msg cond = if cond then Ok () else Error [msg]

let recover f comp =
  let recover_error = function
      Ok v -> Ok (Ok v)
    | (Error err) as res ->
      match f err with
      | None -> res
      | Some vl -> Ok (Error vl) in
  Lwt.map recover_error comp

let sanitize_form_error pp : 'a Dream.form_result Lwt.t -> _ Lwt_result.t =
  fun res ->
  Lwt.map (function
    | `Many_tokens data  -> Error (`FormError ("Many tokens", pp data))
    | `Missing_token data -> Error (`FormError ("Missing token", pp data))
    | `Invalid_token data -> Error (`FormError ("Wrong session", pp data))
    | `Wrong_session data  -> Error (`FormError ("Wrong session", pp data))
    | `Expired (data, _) -> Error (`FormError ("Expired form", pp data))
    | `Wrong_content_type -> Error (`FormError ("Wrong Content Type", "No further information"))
    | `Ok v -> Ok v
  ) res

let handle_register_get ?errors req =
  let token = Dream.csrf_token req in 
  let+ headers = Navigation.build_navigation_bar req in
  tyxml @@ Html.build_page ~headers [Html.Login.register_box ~fields:["dream.csrf", token] ?errors ()]

let handle_register_post req =
  log.info (fun f -> f "register page POST");
  let+ data = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  let res =
    let (let+) x f = Result.(>>=) x f in
    let (and+) x y = match x,y with
        Ok x, Ok y -> Ok (x,y)
      | Error l, Error r -> Error (l @ r)
      | Error e, _ | _, Error e -> Error e in
    let+ username = form_data "username" data |> Result.map_err List.return in
    let+ password = form_data "password" data |> Result.map_err List.return  in
    let+ password2 = form_data "password2" data |> Result.map_err List.return in
    let+ reason = form_data "reason" data  |> Result.map_err List.return in
    let+ () = ensure "username must not be empty" (not @@ String.is_empty username)
    and+ () = ensure "password must not be empty" (not @@ String.is_empty password)
    and+ () = ensure "passwords should match" (String.equal password password2) in
    Ok (username, password, reason) in
  match res with
  | Error errors ->
    List.iter (fun error -> log.debug (fun f -> f "register page invalid form %s" error)) errors;
    handle_register_get ~errors req
  | Ok (username, password, _) ->
    let+ user = Dream.sql req (Database.LocalUser.create_user ~username ~password)
                |> map_err (fun err -> `DatabaseError err) in
    let+ () = Lwt.map Result.return (Dream.invalidate_session req) in
    let+ () = Lwt.map Result.return @@
      Dream.set_session_field req "user" (Database.LocalUser.username user) in
    redirect req "/feed"
  
let handle_login_get ?errors req =
  let token = Dream.csrf_token req in
  let+ headers = Navigation.build_navigation_bar req in
  tyxml @@ Html.build_page ~headers [Html.Login.login_box ~fields:["dream.csrf", token] ?errors ()]
    

let handle_login_post req =
  log.info (fun f -> f "login page POST");
  let+ data = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  let res = 
    let (let+) x f = Result.(>>=) x f in
    let (and+) x y = match x,y with
        Ok x, Ok y -> Ok (x,y)
      | Error l, Error r -> Error (l @ r)
      | Error e, _ | _, Error e -> Error e in
    let+ username = form_data "username" data |> Result.map_err List.return in
    let+ password = form_data "password" data |> Result.map_err List.return in
    let+ () = ensure "username must not be empty" (not @@ String.is_empty username)
    and+ () = ensure "password must not be empty" (not @@ String.is_empty password) in
    Ok (username, password) in  
  match res with
  | Error errors ->
    List.iter (fun error -> log.debug (fun f -> f "login page invalid form %s" error)) errors;
    handle_login_get ~errors req
  | Ok (username, password) ->
    let+ user = Dream.sql req (Database.LocalUser.login_user ~username ~password)
                |> map_err (fun err -> `DatabaseError err) in
    match user with
    | Some user ->
      let+ () = Lwt.map Result.return (Dream.invalidate_session req) in
      let+ () = Lwt.map Result.return @@
        Dream.set_session_field req "user" (Database.LocalUser.username user) in
      redirect req "/feed"
    | None ->
      handle_login_get ~errors:["Invalid username or password"] req

let handle_logout_post req =
  let+ _ = Dream.form ~csrf:false req
           |> sanitize_form_error ([%show: (string * string) list]) in
  let+ () = Dream.invalidate_session req |> Lwt.map Result.return in
  redirect req "/feed"

let route config = 
  Dream.scope "/" [] [
    Dream.scope "/" [check_unauthenticated] [
      Dream.get "/register" @@ Error_handling.handle_error_html config @@ handle_register_get;
      Dream.post "/register" @@ Error_handling.handle_error_html config @@ handle_register_post;
      
      Dream.get "/login" @@ Error_handling.handle_error_html config @@ handle_login_get;
      Dream.post "/login" @@ Error_handling.handle_error_html config @@ handle_login_post;
    ];
    Dream.post "/logout" @@ Error_handling.handle_error_html config @@ handle_logout_post;
  ]
