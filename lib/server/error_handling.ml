open Containers
open Common

let extract_error_details err =
    let status = match err with
      | _ -> `Internal_Server_Error in
    let msg = match err with
      | _ -> "Unknown internal error" in
    let details =
      match err with
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
    tyxml ~status @@
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
    json ~status @@
    `Assoc  [
      "type", `String "error";
      "message", `String msg;
      "details", Option.map_or ~default:`Null (fun v -> `String v) details
    ]

