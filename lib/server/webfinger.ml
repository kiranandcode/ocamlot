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

let is_host_meta_supported_content_type s =
  String.split_on_char ',' s
  |> List.exists (function
      "application/xml"
    | "application/xrd+xml" -> true
    | _ -> false
  )

let handle_webfinger req =
  let content_type = Dream.header req "Accept" in
  if not (Option.for_all is_webfinger_supported_content_type content_type) then
    log.warning (fun f -> f "webfinger for unsupported content type \"%s\", ignoring silently"
                            (Option.value ~default:"" content_type));
  let* queried_resource = Dream.query req "resource"
                          |> lift_opt ~else_:(fun () -> `InvalidWebfinger ("bad query", "missing params")) |> return in
  log.debug (fun f -> f "queried resource: %s" queried_resource);
  match resource_to_username queried_resource with
  | Some username ->
    let username = String.lowercase_ascii username in
    let* user = 
      Dream.sql req begin fun db -> 
        let* query_res = Database.LocalUser.find_user ~username db |> map_err (fun err -> `DatabaseError (Caqti_error.show err)) in
        return @@ lift_opt ~else_:(fun () -> `DatabaseError ("Inconsistent state: user's link failed to resolve")) query_res
      end in
    let result = 
      Database.Interface.Webfinger.construct_query_result_for user
      |> Activitypub.Encode.Webfinger.query_result in
    log.debug (fun f -> f "result for webfinger query: %a" Yojson.Safe.pp result);
    Web.json result
  | None ->
    log.debug (fun f -> f "user %s was not found" queried_resource);
    Web.not_found_json "User was not found"

let handle_host_meta req =
  let content_type = Dream.header req "Accept" in
  if not (Option.for_all is_host_meta_supported_content_type content_type) then
    log.warning (fun f -> f "host-meta for unsupported content type \"%s\", ignoring silently"
                            (Option.value ~default:"" content_type));
  let host_meta = Format.sprintf {|
<?xml version="1.0" encoding="UTF-8"?>
<XRD xmlns="http://docs.oasis-open.org/ns/xri/xrd-1.0">
    <Link rel="lrdd" template="%s/.well-known/webfinger?resource={uri}" type="application/json" />
</XRD>
|} (Uri.to_string @@ Lazy.force Configuration.domain) in
  Web.xrd_xml host_meta

let handle_well_known_nodeinfo req =
  let content_type = Dream.header req "Accept" in
  if not (Option.for_all is_webfinger_supported_content_type content_type) then
    log.warning (fun f -> f "node-info for unsupported content type \"%s\", ignoring silently"
                            (Option.value ~default:"" content_type));

  Web.json (`Assoc [
    "links", `List [
      `Assoc [
        "rel", `String "http://nodeinfo.diaspora.software/ns/schema/2.0";
        "href", `String (Uri.to_string @@ Lazy.force Configuration.Url.node_info_path);
      ]
    ]
  ])

let handle_nodeinfo req =
  let content_type = Dream.header req "Accept" in
  if not (Option.for_all is_webfinger_supported_content_type content_type) then
    log.warning (fun f -> f "node-info for unsupported content type \
                             \"%s\", ignoring silently"
                            (Option.value ~default:"" content_type));
  let software  : Activitypub.Types.Nodeinfo.software = {
    name = "ocamlot";
    version = "%%VERSION%%";
  } in
  let* total_posts = Web.sql req (Database.Posts.count_local) in
  let* users_count = Web.sql req (Database.LocalUser.local_user_count) in
  let* registration_allowed = Web.sql req (Database.Admin.is_registration_allowed) in
  let users : Activitypub.Types.Nodeinfo.usage_users = {
    total=users_count;
    active_month=users_count;
    active_half_year=users_count;
  } in
  let usage  : Activitypub.Types.Nodeinfo.usage = {
    local_posts=total_posts;
    users;
  } in
  let node_info: Activitypub.Types.Nodeinfo.t =  {
    software;
    protocols=["activitypub"];
    inbound_services=[];
    outbound_services=[];
    usage;
    open_registrations=registration_allowed;
    metadata = Some (`Assoc []);
    raw = `Null
  } in

  Web.json (Activitypub.Encode.Nodeinfo.t node_info)


let route =
  Dream.scope "/.well-known" [] [
    Dream.get "/webfinger" @@ Error_display.handle_error_json handle_webfinger;
    Dream.get "/host-meta" @@ Error_display.handle_error_json handle_host_meta;
    Dream.get "/nodeinfo" @@ Error_display.handle_error_json handle_well_known_nodeinfo;
  ]

let node_info = Dream.get "/nodeinfo/2.0" @@ Error_display.handle_error_json handle_nodeinfo;
