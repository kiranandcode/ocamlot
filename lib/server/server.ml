[@@@warning "-33"]


let handle_get_home req =
  Common.with_current_user req @@ fun user -> Dream.html (Html.Home.build user req)



let () =
  let config = Configuration.Params.create ~domain:"ocamlot.nfshost.com" in
  Dream.run ~tls:false ~port:4000
  @@ Dream.logger
  @@ Dream.sql_pool "sqlite3://:../../test.db"
  @@ Dream.sql_sessions 
  @@ Dream.router [
    Webfinger.route config;
    
    Authentication.route;

    Actor.route config;

    Activity.route config;

    Dream.get "/home" @@ handle_get_home;

    Dream.get "/static/**" @@ Dream.static "static";
    Dream.get "/**" @@ fun req -> Dream.redirect req "/home"
  ]
