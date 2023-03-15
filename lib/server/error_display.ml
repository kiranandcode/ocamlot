open Containers

let log = Logging.add_logger "error.display"

(* * Error Display  *)
(* ** Html *)
let handle_error_html handler req =
  Lwt.bind (handler req) @@ fun result ->
  match result with
  | Ok result -> Lwt.return result
  | Error err ->
    let status, msg, details = Error_handling.extract_error_details err in
    let details =
      Option.return_if (Lazy.force Configuration.debug) details in
    Web.tyxml_pure ~status @@
    View.Page.render_page "OCamlot - Error" [
      View.Components.render_heading
        ~icon:"E" ~current:("Internal Server Error")
        ();
      Tyxml.(Html.div ~a:[Html.a_class ["error-box"; "error-details"]] [
          Html.p [Html.txt msg];
          Html.code [
            Html.txt (match details with
                | None -> "No further details..."
                | Some details -> details)
          ]
        ])
    ]

(* ** Json *)
let handle_error_json handler req =
  Lwt.bind (handler req) @@ fun result ->
  match result with
  | Ok result -> Lwt.return result
  | Error err ->
    let status, msg, details = Error_handling.extract_error_details err in
    let details =
      Option.return_if (Lazy.force Configuration.debug) details in
    log.debug (fun f -> f "ERROR: %s - %a" msg (Option.pp String.pp) details);
    Web.json_pure ~status @@
    `Assoc  [
      "type", `String "error";
      "message", `String msg;
      "details", Option.map_or ~default:`Null (fun v -> `String v) details
    ]
