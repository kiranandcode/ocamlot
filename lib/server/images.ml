[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "web.image"

let database =
  Conan.Process.database ~tree:(Conan_magic_database.tree)

let get_image_extension mime_type =
  List.Assoc.get ~eq:(String.equal)  mime_type 
    Configuration.Features.supported_images

let build_image_path config ~image_name:image_base_name =
  let+ user_image_path =
    Fpath.of_string
      (Configuration.Params.user_image_path config)
    |> Result.map_err (fun (`Msg err) ->
        `InternalError ("invalid user image path", err))
    |> Lwt_result.lift in
  Fpath.(user_image_path / image_base_name)
  |> Fpath.to_string
  |> Lwt.return_ok
  
let upload_file config req ~fname:_ ~data =
  let+ meta_data =
    Conan_string.run ~database data
    |> Result.map_err (fun (`Msg m) -> `Internal ("mime lookup error", m))
    |> Lwt_result.lift in
  let+ mime_type = Conan.Metadata.mime meta_data
             |> (function (Some meta_data) -> Ok (meta_data)
                        | None ->
                          Error (`InvalidData "could not obtain mime \
                                               type for data") )
             |> Lwt_result.lift in
  let+ image_extension = get_image_extension mime_type
                      |> Result.of_opt
                      |> Result.map_err (fun _ ->
                          `UnsupportedFormat (mime_type ^ "is not a \
                                                           supported \
                                                           image type."))
                      |> Lwt_result.lift in
  let image_base_name = Uuidm.v `V4 |> Uuidm.to_string in
  let image_name = image_base_name ^ "." ^ image_extension in
  let hash = Mirage_crypto.Hash.digest `SHA256 (Cstruct.of_string data)
             |> Cstruct.to_string in
  (* insert the image into our database *)
  let+ _ =
    Dream.sql req (Database.UserImage.create ~hash ~path:image_name)
    |> Lwt_result.map_error (fun err ->
        `DatabaseError (Caqti_error.show err)) in
  (* write image to disk *)
  let+ () =
    let+ path = build_image_path config ~image_name in
    Lwt_io.with_file
      ~mode:Lwt_io.Output ~flags:Unix.[O_CREAT; O_EXCL; O_WRONLY]
      path (fun oc ->
          Lwt_io.write oc data
          |> Lwt_result.ok) in
  Lwt_result.return (image_name)


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
  Dream.scope "/images" [] [
    Dream.get "/:image" @@ Error_handling.handle_error_html config @@ handle_image_get config;
  ]


