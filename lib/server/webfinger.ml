open Containers
open Common

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
    Dream.log "webfinger for unsupported content type \"%s\", ignoring silently"
      (Option.value ~default:"" content_type);
  let> queried_resource = Dream.query req "resource" |> or_bad_reqeust in

  match resource_to_username config queried_resource with
  | Some username ->
    Dream.sql req begin fun db -> 
      let+ query_res = Database.LocalUser.lookup_user ~username db in
      let> local_user_opt = query_res |> or_error in
      let> local_user = local_user_opt |> or_not_found in
      let json =
        Database.Interface.Webfinger.construct_query_result_for config local_user
        |> Activitypub.Encode.Webfinger.query_result
        |> Yojson.Safe.to_string in
      Dream.json json
    end
  | None ->
    not_found ()

let route config =
  Dream.scope "/.well-known" [] [
    Dream.get "/webfinger" (handle_webfinger config)
  ]
