open Containers
let (let+) x f = Lwt.bind x f

type t = Dream.middleware

let handle_get_home req =
  Common.with_current_user req @@ fun user -> Dream.html (Html.Home.build user req)

let () =
  Dream.initialize_log ~level:`Debug ();
  Dream.run ~port:4444
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3://:test.db"
  @@ Dream.sql_sessions 
  @@ Dream.router [
    Authentication.route;

    Actor.route;

    Dream.get "/home" @@ handle_get_home;

    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/home"
  ]
