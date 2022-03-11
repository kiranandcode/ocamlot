open Containers

let internal_error = Dream.respond ~status:`Internal_Server_Error "Internal server error"
let bad_request = Dream.respond ~status:`Bad_Request "Bad Request"
let not_found ?(msg="Not found") () = Dream.respond ~status:`Not_Found msg

let (let+) x f = x f
let (let*) x f = Lwt.bind x f

let or_error vl knt = 
  match vl with
  | Ok vl -> knt vl
  | Error e ->
    Dream.error (fun log -> log "error: %s" e);
    internal_error

let or_else ~else_ vl knt = 
  match vl with
  | Some vl -> knt vl
  | _ -> else_

let or_not_found ?msg vl knt =
  or_else ~else_:(not_found ?msg ()) vl knt

let or_bad_reqeust vl knt =
  or_else ~else_:bad_request vl knt

let holds_or_bad_request vl knt =
  or_bad_reqeust (if vl then Some () else None) knt

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
  let+ queried_resource = Dream.query req "resource" |> or_bad_reqeust in

  match resource_to_username config queried_resource with
  | Some username ->
    Dream.sql req begin fun db -> 
      let* query_res = Database.LocalUser.lookup_user ~username db in
      let+ local_user_opt = query_res |> or_error in
      let+ local_user = local_user_opt |> or_not_found in
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
