open Containers
let (let+) x f = Lwt.bind x f

let is_not_empty opt = opt |> Option.filter (Fun.negate String.is_empty)
let check_unauthenticated = Common.Middleware.redirect_if_present "user" ~to_:"/home"

let handle_register_get request =
  let+ errors = Common.get_errors request in
  let errors = 
    errors
    |> Option.map List.return
    |> Option.value ~default:[] in
  Dream.html (Html.Register.build ~errors request)


let handle_register_post req =
  Dream.log "register page POST";
  let+ result = Dream.form req in
  let fail_with err =
    let+ () = Common.set_error req err in
    Dream.redirect req "/register" in
  let (let-@!) (x,err) f = match x with Error str -> fail_with (err ^ str) | Ok vl -> f vl in
  let (let-!) (x,err) f = match x with  None -> fail_with err | Some vl -> f vl in
  let (let-?) (x,err) f = match x with false -> fail_with err | true -> f () in
  match result with
  | `Ok elts ->
    let-! username = List.assoc_opt ~eq:String.equal "username" elts |> is_not_empty, "Username can not be empty" in
    let-! password = List.assoc_opt ~eq:String.equal "password" elts |> is_not_empty, "Password can not be empty" in
    let passwords = List.filter (function "password", _ -> true | _ -> false ) elts in
    let-? () = passwords |> List.for_all (fun (_, vl) -> String.(vl = password)), "Passwords do not match"  in
    let+ result = Dream.sql req @@ fun db ->
      Database.User.create_user ~username ~password db in
    let-@! user = result, "Error creating user: " in
    let+ () = Dream.invalidate_session req in
    let+ () = Dream.set_session_field req "user"
                (Database.User.username user) in
    Dream.redirect req "/home"
  | _ ->
    Dream.redirect req "/register"


let handle_login_get req =
  let+ errors = Common.get_errors req in
  let errors = 
    errors
    |> Option.map List.return
    |> Option.value ~default:[] in
  Dream.html (Html.Login.build ~errors req)


let handle_login_post req =
  let+ result = Dream.form req in
  let fail_with err =
    let+ () = Common.set_error req err in
    Dream.redirect req "/login" in
  let (let-@!) (x,err) f = match x with Error str -> fail_with (err ^ str) | Ok vl -> f vl in
  let (let-!) (x,err) f = match x with  None -> fail_with err | Some vl -> f vl in
  match result with
  | `Ok elts ->
    let-! username = List.assoc_opt ~eq:String.equal "username" elts |> is_not_empty, "Username can not be empty" in
    let-! password = List.assoc_opt ~eq:String.equal "password" elts |> is_not_empty, "Password can not be empty" in
    let+ result = Dream.sql req @@ fun db -> Database.User.login_user ~username ~password db in
    let-@! user = result, "Internal error while logging in: " in
    let-! user = user, "Could not log in - user not found/password does not match" in
    let+ () = Dream.invalidate_session req in
    let+ () = Dream.set_session_field req "user"
                (Database.User.username user) in
    Dream.redirect req "/home"
  | _ ->
    Dream.redirect req "/login"

let handle_logout_post req =
  let+ result = Dream.form req in
  match result with
  | `Ok _ ->
    let+ () = Dream.invalidate_session req in
    Dream.redirect req "/home"
  | _ ->
    Dream.redirect req "/home"

let route = 
    Dream.scope "/" [] [

      Dream.scope "/" [check_unauthenticated] [
        Dream.get "/register" handle_register_get;
        Dream.post "/register" handle_register_post;

        Dream.get "/login" handle_login_get;
        Dream.post "/login" handle_login_post;
      ];
      Dream.post "/logout" handle_logout_post;
    ]
