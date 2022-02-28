open Containers
let (let+) x f = Lwt.bind x f

let handle_actor_get req =
  Dream.sql req @@ fun db ->
  let username = Dream.param req "username" in
  let+ user = Database.User.create_user ~username ~password:"I am a dumbdumb" db in
  match user  with
  | Ok user ->
    let text = Format.sprintf "Created user: %s\n username: %s\npublic key: %s"
                 user.username
                 (Option.value ~default:"NONE" user.display_name)
                 (Format.to_string X509.Public_key.pp user.pubkey) in
    Dream.html (text)
  | Error err -> Dream.html ("failed to create user because: " ^ err)

let handle_inbox_get req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let handle_inbox_post req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let handle_outbox_get req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let handle_outbox_post req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let is_not_empty opt = opt |> Option.filter (Fun.negate String.is_empty)

let get_errors request = 
  let error = Dream.session_field request "errors"
              |> is_not_empty in
  let+ () = Dream.set_session_field request "errors" "" in
  Lwt.return error

let set_error request error = Dream.set_session_field request "errors" error

let request_bind request result f =
  let+ result = result in 
  match result with
  | Error str ->
    let+ () = set_error request str in
    Dream.redirect request "/error"
  | Ok vl -> f vl
  

let with_current_user request f =
  let (let@) x f = request_bind request x f in
  match Dream.session_field request "user" with
  | Some username ->
    let@ user = Dream.sql request @@ Database.User.lookup_user ~username in
    f (Some user)
  | None -> f None
      

let handle_register_get request =
  Dream.debug (fun log -> log ~request "register");
  match Dream.session_field request "user" with
  | Some _ ->
    Dream.redirect request "/home"
  | None ->
    let+ errors = get_errors request in
    let errors = 
      errors
      |> Option.map List.return
      |> Option.value ~default:[] in
    Dream.html (Html.Register.build ~errors request)

let handle_register_post req =
  Dream.log "register page POST";
  let+ result = Dream.form req in
  let fail_with err =
    let+ () = set_error req err in
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
    let+ () = Dream.set_session_field req "user" user.username in
    Dream.redirect req "/home"
  | _ ->
    Dream.redirect req "/register"

let handle_login_get req = Dream.html (Html.Login.build req)

let handle_login_post req =
  let+ result = Dream.form req in
  let fail_with err =
    let+ () = set_error req err in
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
    let+ () = Dream.set_session_field req "user" user.username in
    Dream.redirect req "/home"
  | _ ->
    Dream.redirect req "/login"

let handle_get_home req =
  with_current_user req @@ fun user -> Dream.html (Html.Home.build user req)

let handle_logout_post req =
  let+ result = Dream.form req in
  match result with
  | `Ok _ ->
    let+ () = Dream.invalidate_session req in
    Dream.redirect req "/home"
  | _ ->
    Dream.redirect req "/home"

let () =
  Dream.initialize_log ~level:`Debug ();
  Dream.run ~port:4444
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3://:test.db"
  @@ Dream.sql_sessions 
 @@ Dream.router [
    Dream.get "/register" handle_register_get;
    Dream.post "/register" handle_register_post;

    Dream.get "/login" handle_login_get;
    Dream.post "/login" handle_login_post;

    Dream.post "/logout" handle_logout_post;


    Dream.get "/user/:username" handle_actor_get;
    Dream.get "/user/:username/inbox" handle_inbox_get;
    Dream.post "/user/:username/inbox" handle_inbox_post;
    Dream.get "/user/:username/outbox" handle_outbox_get;
    Dream.post "/user/:username/outbox" handle_outbox_post;
    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/home" @@ handle_get_home;
    Dream.get "/**" @@ fun req -> Dream.redirect req "/home"
  ]
