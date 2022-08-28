[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "web.finger"


let resource_to_username config queried_resource =
  let webfinger_format = Configuration.Regex.webfinger_format config |> Re.compile in
  let local_user_id_format = Configuration.Regex.local_user_id_format config |> Re.compile in
  Re.exec_opt webfinger_format queried_resource
  |> Fun.flip Option.bind (Fun.flip Re.Group.get_opt 1)
  |> Option.or_lazy ~else_:begin fun () ->
    Re.exec_opt local_user_id_format queried_resource
    |> Fun.flip Option.bind (Fun.flip Re.Group.get_opt 1)
  end
  
let handle_webfinger config req =
  let content_type = Dream.header req "Accept" in
  if not (Option.for_all String.(prefix ~pre:"application/json") content_type) then
    log.warning (fun f -> f "webfinger for unsupported content type \"%s\", ignoring silently"
                            (Option.value ~default:"" content_type));
  let+ queried_resource = Dream.query req "resource" |> lift_opt ~else_:(fun () -> `InvalidWebfinger ("bad query", "missing params")) |> return in

  match resource_to_username config queried_resource with
  | Some username ->
    let+ user = 
      Dream.sql req begin fun db -> 
        let+ query_res = Database.LocalUser.lookup_user ~username db |> map_err (fun err -> `DatabaseError err) in
        return @@ lift_opt ~else_:(fun () -> `DatabaseError ("Inconsistent state: user's link failed to resolve")) query_res
      end in
    Database.Interface.Webfinger.construct_query_result_for config user
    |> Activitypub.Encode.Webfinger.query_result
    |> activity_json
  | None ->
    not_found_json "User was not found"

let route config =
  Dream.scope "/.well-known" [] [
    Dream.get "/webfinger" @@ Error_handling.handle_error_json config @@ (handle_webfinger config)
  ]
