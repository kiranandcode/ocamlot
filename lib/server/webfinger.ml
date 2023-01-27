[@@@warning "-33"]
open Containers
open Common

let log = Logging.add_logger "web.finger"


let resource_to_username queried_resource =
  let webfinger_format = Lazy.force (Configuration.Regex.webfinger_format) in
  let local_user_id_format = Lazy.force (Configuration.Regex.local_user_id_format) in
  Re.exec_opt  webfinger_format queried_resource
  |> Fun.flip Option.bind (Fun.flip Re.Group.get_opt 1)
  |> Option.or_lazy ~else_:begin fun () ->
    Re.exec_opt local_user_id_format queried_resource
    |> Fun.flip Option.bind (Fun.flip Re.Group.get_opt 1)
  end

let is_webfinger_supported_content_type s =
  String.split_on_char ',' s
  |> List.exists (function
      "application/json"
      | "application/jrd+json" -> true
      | _ -> false
    )
  
let handle_webfinger req =
  let content_type = Dream.header req "Accept" in
  if not (Option.for_all is_webfinger_supported_content_type content_type) then
    log.warning (fun f -> f "webfinger for unsupported content type \"%s\", ignoring silently"
                            (Option.value ~default:"" content_type));
  let+ queried_resource = Dream.query req "resource"
                          |> lift_opt ~else_:(fun () -> `InvalidWebfinger ("bad query", "missing params")) |> return in
  log.debug (fun f -> f "queried resource: %s" queried_resource);
  match resource_to_username queried_resource with
  | Some username ->
    let+ user = 
      Dream.sql req begin fun db -> 
        let+ query_res = Database.LocalUser.find_user ~username db |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
        return @@ lift_opt ~else_:(fun () -> `DatabaseError ("Inconsistent state: user's link failed to resolve")) query_res
      end in
    let result = 
      Database.Interface.Webfinger.construct_query_result_for user
      |> Activitypub.Encode.Webfinger.query_result in
    log.debug (fun f -> f "result for webfinger query: %a" Yojson.Safe.pp result);
    json result
  | None ->
    log.debug (fun f -> f "user %s was not found" queried_resource);
    not_found_json "User was not found"

let route =
  Dream.scope "/.well-known" [] [
    Dream.get "/webfinger" @@ Error_handling.handle_error_json handle_webfinger
  ]
