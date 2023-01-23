[@@@warning "-33"]
open Containers
open Common


let handle_image_get config req =
  let image = Dream.param req "image" in
  let+ image = Dream.sql req (Database.UserImage.find_by_path ~path:image)
               |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
  match image with
  | None -> respond ~status:`Not_Found "Not found"
  | Some image ->
    let local_root = Configuration.Params.user_image_path config in
    file ~local_root image.path



let route config =
  Dream.scope "/images" [Common.Middleware.enforce_present_resource "user"] [
    Dream.get "/:image" @@ Error_handling.handle_error_html config @@ handle_image_get config;
  ]


