let main_page _ =
  Lwt.return @@ Sihl.Web.Response.of_html View.Hello.page

let redirect_to_frontend _ =
  Lwt.return @@ Sihl.Web.Response.redirect_to "/site"
