[@@@warning "-33"]
open Containers

let handle_get_home req =
  Common.with_current_user req @@ fun user -> Dream.html (Html.Home.build user req)



let () =
  let config = Settings.create ~domain:"ocamlot.xyz" in
  Dream.initialize_log ~level:`Debug ();
  Dream.run
    ~tls:false
    ~port:4000
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3://:test.db"
  @@ Dream.sql_sessions 
  @@ Dream.router [
    Authentication.route;
    Dream.scope "/api" [] [
      Actor.route config;
    ];


    Dream.get "/home" @@ handle_get_home;

    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/home"
  ]
