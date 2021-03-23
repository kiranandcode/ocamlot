
let get_actor (req: Rock.Request.t) : Rock.Response.t Lwt.t =
  let actor = Sihl.Web.Router.param req "name" in
  let body = Rock.Body.of_string actor in
  (* TODO: IMPLEMENT *)
  Lwt.return @@ Rock.Response.make ~body ()
