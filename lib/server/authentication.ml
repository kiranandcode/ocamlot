[@@@warning "-33"]
open Containers
open Common

let log = Dream.sub_log "web.auth"

let check_unauthenticated = Common.Middleware.redirect_if_present "user" ~to_:"/home"


let handle_register_get ?errors req =
  let token = Dream.csrf_token req in
  tyxml @@ Html.build_page [Html.Login.register_box ~fields:["dream.csrf", token] ?errors ()]

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



let handle_register_post req =
  log.info (fun f -> f "register page POST");
  let+ result = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  log.info (fun f -> f "data was %s" ([%show: (string * string) list] result));
  Lwt_result.fail (`Msg "Todo")
  

    

(* let handle_register_post req =
 *   Dream.log "register page POST";
 *   let+ result = Dream.form req in
 *   match result with
 *   | `Ok elts ->
 *     let> username = List.assoc_opt ~eq:String.equal "username" elts
 *                     |> or_else ~to_:"/register" ~req "username can not be empty" in
 *     let> password = List.assoc_opt ~eq:String.equal "password" elts
 *                     |> or_else ~to_:"/register" ~req "Password can not be empty" in
 *     let passwords = List.filter (function "password", _ -> true | _ -> false ) elts in
 *     let> () = passwords
 *               |> List.for_all (fun (_, vl) -> String.(vl = password))
 *               |> holds_or_else ~to_:"/register" ~req "Passwords do not match"  in
 *     let+ result = Dream.sql req @@ fun db ->
 *       Database.LocalUser.create_user ~username ~password db in
 *     let> user = result |> ok_or_else ~to_:"/register" ~req "Error creating user: " in
 *     let+ () = Dream.invalidate_session req in
 *     let+ () = Dream.set_session_field req "user"
 *                 (Database.LocalUser.username user) in
 *     Dream.redirect req "/home"
 *   | _ ->
 *     Dream.redirect req "/register" *)


(* let handle_login_get req =
 *   let+ errors = Common.Error.get req in
 *   let _errors = 
 *     errors
 *     |> Option.map List.return
 *     |> Option.value ~default:[] in
 *   Dream.html (invalid_arg "TODO") (\* (Html.Login.build ~errors req) *\) *)


(* let handle_login_post req =
 *   let+ result = Dream.form req in
 *   match result with
 *   | `Ok elts ->
 *     let> username = List.assoc_opt ~eq:String.equal "username" elts
 *                     |> or_else ~to_:"/login" ~req "Username can not be empty" in
 *     let> password = List.assoc_opt ~eq:String.equal "password" elts
 *                     |> or_else ~to_:"/login" ~req "Password can not be empty" in
 *     let+ result = Dream.sql req @@ fun db -> Database.LocalUser.login_user ~username ~password db in
 *     let> user = result |> ok_or_else ~to_:"/login" ~req "Internal error while logging in: " in
 *     let> user = user |> or_else ~to_:"/login" ~req "Could not log in - user not found/password does not match" in
 *     let+ () = Dream.invalidate_session req in
 *     let+ () = Dream.set_session_field req "user"
 *                 (Database.LocalUser.username user) in
 *     Dream.redirect req "/home"
 *   | _ ->
 *     Dream.redirect req "/login" *)

(* let handle_logout_post req =
 *   let+ result = Dream.form req in
 *   match result with
 *   | `Ok _ ->
 *     let+ () = Dream.invalidate_session req in
 *     Dream.redirect req "/home"
 *   | _ ->
 *     Dream.redirect req "/home" *)

let route config = 
  Dream.scope "/" [] [
    Dream.scope "/" [check_unauthenticated] [
      Dream.get "/register" handle_register_get;
      Dream.post "/register" @@ Error_handling.handle_error_html config @@ handle_register_post;
      
      (* Dream.get "/login" handle_login_get;
       * Dream.post "/login" handle_login_post; *)
    ];
    (* Dream.post "/logout" handle_logout_post; *)
  ]
