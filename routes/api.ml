(* All the JSON HTTP entry points are listed here.

   Don't put actual logic here to keep it declarative and easy to read. The
   overall scope of the web app should be clear after scanning the routes. *)

let redirect_to_frontend =
  Sihl.Web.Http.get "" Handler.Page.redirect_to_frontend

let actor_endpoint =
  Sihl.Web.Http.get ":name" Handler.Api.get_actor


let middlewares = [ Sihl.Web.Middleware.json; Sihl.Web.Middleware.bearer_token ]
let router = Sihl.Web.Http.router ~middlewares ~scope:"/" [
    redirect_to_frontend;
    actor_endpoint
  ]
