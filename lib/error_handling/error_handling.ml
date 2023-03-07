open Containers
open Common

let log = Logging.add_logger "web.error"

(* * Extract error details *)

let extract_error_details err =
    let status : Dream.status = match err with
    | `UserNotFound _ -> `Not_Found
    | `ActivityNotFound _ -> `Not_Found
    | `Unauthorised _ ->  `Forbidden
    | `InvalidActivitypubObject _
    | `InvalidSignature
    | `InvalidData _ -> `Bad_Request
    | `UnsupportedFormat _ -> `Bad_Request
    | `InputTooLarge (_, _, _) -> `Bad_Request
    | _ -> `Internal_Server_Error in
    let msg = match err with
      | `InvalidActivitypubObject _ -> "Invalid activitypub object"
      | `InvalidSignature -> "Invalid Signature"
      | `Unauthorised s -> s
      | `InvalidData _ -> "Invalid Data"
      | `UnsupportedFormat msg -> "Unsupported format " ^ msg
      | `UserNotFound (_) -> "User not found"
      | `ActivityNotFound (_) -> "Activity not found"
      | `InvalidWebfinger (title, _) -> "Web finger error - " ^ title
      | `InputTooLarge (msg, size, _) ->
        "User input too large -" ^ msg ^ " (max size is " ^
        string_of_int size ^ ")"
      | `DatabaseError _ -> "Database error"
      | `FormError (title, _) -> "Form error - " ^ title
      | _ -> "Unknown internal error" in
    let details =
      match err with
      | `InvalidActivitypubObject msg -> msg
      | `InvalidData msg -> msg
      | `UserNotFound name -> "User " ^ name ^ " not found"
      | `ActivityNotFound name -> "Activity " ^ name ^ " not found"
      | `InvalidWebfinger (_, msg) -> "Query data was:\n  " ^ msg
      | `InputTooLarge (_, _, details) -> "Input details: " ^ details
      | `UnsupportedFormat _ -> "No further details "
      | `DatabaseError msg -> "Error was:\n" ^ msg
      | `FormError (_, data) -> "Form data was:\n" ^ data
      | `Msg m -> m
      | `InvalidSignature -> "Invalid signature"
      | `Internal (msg, err) -> msg ^ ": " ^ err
      | _ -> "No Further Details" in
    (status, msg, details)


(* * Error Display  *)
(* ** Html *)
let handle_error_html handler req =
  Lwt.bind (handler req) @@ fun result ->
  match result with
  | Ok result -> Lwt.return result
  | Error err ->
    let status, msg, details = extract_error_details err in
    let details =
      Option.return_if (Lazy.force Configuration.debug) details in
    tyxml_pure ~status @@
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
    let status, msg, details = extract_error_details err in
    let details =
      Option.return_if (Lazy.force Configuration.debug) details in
    log.debug (fun f -> f "ERROR: %s - %a" msg (Option.pp String.pp) details);
    json_pure ~status @@
    `Assoc  [
      "type", `String "error";
      "message", `String msg;
      "details", Option.map_or ~default:`Null (fun v -> `String v) details
    ]
