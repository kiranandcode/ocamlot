open Containers
open Common

let log = Logging.add_logger "web.error"

let extract_error_details err =
    let status = match err with
    | `UserNotFound _ -> `Not_Found
    | `ActivityNotFound _ -> `Not_Found
    | `InvalidSignature
    | `InvalidData _ -> `Bad_Request
    | _ -> `Internal_Server_Error in
    let msg = match err with
      | `InvalidSignature -> "Invalid Signature"
      | `InvalidData _ -> "Invalid Data"
      | `UserNotFound (_) -> "User not found"
      | `ActivityNotFound (_) -> "Activity not found"
      | `InvalidWebfinger (title, _) -> "Web finger error - " ^ title
      | `DatabaseError _ -> "Database error"
      | `FormError (title, _) -> "Form error - " ^ title
      | _ -> "Unknown internal error" in
    let details =
      match err with
      | `InvalidData msg -> msg
      | `UserNotFound name -> "User " ^ name ^ " not found"
      | `ActivityNotFound name -> "Activity " ^ name ^ " not found"
      | `InvalidWebfinger (_, msg) -> "Query data was:\n  " ^ msg
      | `DatabaseError msg -> "Error was:\n" ^ msg
      | `FormError (_, data) -> "Form data was:\n" ^ data
      | `Msg m -> m
      | _ -> "No Further Details" in
    (status, msg, details)

let handle_error_html config handler req =
  Lwt.bind (handler req) @@ fun result ->
  match result with
  | Ok result -> Lwt.return result
  | Error err ->
    let status, msg, details = extract_error_details err in
    let details =
      Option.return_if (Configuration.Params.debug config) details in
    tyxml_pure ~status @@
    Tyxml.Html.html
      Html.(head Tyxml.Html.(txt "OCamlot - Error"))
      (Tyxml.Html.body [
         Html.Error.error ?details msg
       ])

let handle_error_json config handler req =
  Lwt.bind (handler req) @@ fun result ->
  match result with
  | Ok result -> Lwt.return result
  | Error err ->
    let status, msg, details = extract_error_details err in
    let details =
      Option.return_if (Configuration.Params.debug config) details in
    log.debug (fun f -> f "ERROR: %s - %a" msg (Option.pp String.pp) details);
    json_pure ~status @@
    `Assoc  [
      "type", `String "error";
      "message", `String msg;
      "details", Option.map_or ~default:`Null (fun v -> `String v) details
    ]
