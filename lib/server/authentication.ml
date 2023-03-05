[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "web.auth"

let check_unauthenticated = Common.Middleware.redirect_if_present "user" ~to_:"/feed"

(* * Utilities *)

let recover f comp =
  let recover_error = function
      Ok v -> Ok (Ok v)
    | (Error err) as res ->
      match f err with
      | None -> res
      | Some vl -> Ok (Error vl) in
  Lwt.map recover_error comp

(* * Register *)
(* ** Get *)
let handle_register_get ?errors:_ req =
  let token = Dream.csrf_token req in 
  let* headers, action = Navigation.build_navigation_bar req in
  tyxml @@
  View.Page.render_page "Register a new account" [
    View.Header.render_header ?action headers;
    View.Login_box.render_register_box ~fields:["dream.csrf", token] ();
    Tyxml.Html.div ~a:[Tyxml.Html.a_class ["login-info"; "markdown"]]
      (Markdown.markdown_to_html (Lazy.force Configuration.about_this_instance))
  ]

(* ** Post *)
let handle_register_post req =
  log.info (fun f -> f "register page POST");
  let* data = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  let res =
    let open VResult in
    let* username = form_data "username" data |> Result.map_err List.return in
    let* password = form_data "password" data |> Result.map_err List.return  in
    let* password2 = form_data "password2" data |> Result.map_err List.return in
    let* reason = form_data "reason" data  |> Result.map_err List.return in
    let* () = ensure "username must not be empty" (not @@ String.is_empty username)
    and* () = ensure "password must not be empty" (not @@ String.is_empty password)
    and* () = ensure "passwords should match" (String.equal password password2) in
    Ok (username, password, reason) in
  match res with
  | Error errors ->
    List.iter (fun error -> log.debug (fun f -> f "register page invalid form %s" error)) errors;
    handle_register_get ~errors req
  | Ok (username, password, _) ->
    let* user = Dream.sql req (Database.LocalUser.create_user ~username ~password)
                |> map_err (function
                      `ArgonError err -> `ArgonError err 
                    | #Caqti_error.t as err -> `DatabaseError (Caqti_error.show err)) in
    let* () = Lwt.map Result.return (Dream.invalidate_session req) in
    let* () = Lwt.map Result.return @@
      Dream.set_session_field req "user" (user.Database.LocalUser.username) in
    redirect req "/feed"

(* * Login *)  
(* ** Get *)
let handle_login_get ?errors:_ req =
  let token = Dream.csrf_token req in
  let* headers, action = Navigation.build_navigation_bar req in
  tyxml @@ View.Page.render_page "Log in to your account" [
    View.Header.render_header ?action headers;
    View.Login_box.render_login_box ~fields:["dream.csrf", token] ();
    Tyxml.Html.div ~a:[Tyxml.Html.a_class ["login-info"; "markdown"]]
      (Markdown.markdown_to_html (Lazy.force Configuration.about_this_instance))
  ]

(* ** Post *)
let handle_login_post req =
  log.info (fun f -> f "login page POST");
  let* data = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  let res =
    let open VResult in
    let* username = form_data "username" data |> Result.map_err List.return in
    let* password = form_data "password" data |> Result.map_err List.return in
    let* () = ensure "username must not be empty" (not @@ String.is_empty username)
    and* () = ensure "password must not be empty" (not @@ String.is_empty password) in
    Ok (username, password) in  
  match res with
  | Error errors ->
    List.iter (fun error -> log.debug (fun f -> f "login page invalid form %s" error)) errors;
    handle_login_get ~errors req
  | Ok (username, password) ->
    let* user = Dream.sql req (Database.LocalUser.login_user ~username ~password)
                |> map_err (function
                      `ArgonError err -> `ArgonError err
                    | #Caqti_error.t as err -> `DatabaseError (Caqti_error.show err)) in
    match user with
    | Some user ->
      let* () = Lwt.map Result.return (Dream.invalidate_session req) in
      let* () = Lwt.map Result.return @@
        Dream.set_session_field req "user" (user.Database.LocalUser.username) in
      redirect req "/feed"
    | None ->
      handle_login_get ~errors:["Invalid username or password"] req

(* * Logout *)
let handle_logout_post req =
  let* _ = Dream.form ~csrf:false req
           |> sanitize_form_error ([%show: (string * string) list]) in
  let* () = Dream.invalidate_session req |> Lwt.map Result.return in
  redirect req "/feed"

(* * Router *)
let route = 
  Dream.scope "/" [] [
    Dream.scope "/" [check_unauthenticated] [
      Dream.get "/register" @@ Error_handling.handle_error_html @@ handle_register_get;
      Dream.post "/register" @@ Error_handling.handle_error_html @@ handle_register_post;

      Dream.get "/login" @@ Error_handling.handle_error_html @@ handle_login_get;
      Dream.post "/login" @@ Error_handling.handle_error_html @@ handle_login_post;
    ];
    Dream.post "/logout" @@ Error_handling.handle_error_html @@ handle_logout_post;
  ]
