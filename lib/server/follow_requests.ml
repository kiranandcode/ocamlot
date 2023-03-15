[@@@warning "-33"]
open Containers
open Common

let handle_follow_requests_get _req =
  return (Error (`NotImplemented "handle follow requests get"))

let handle_follow_requests_accept _req =
  return (Error (`NotImplemented "handle follow requests accept"))

let handle_follow_requests_reject _req =
  return (Error (`NotImplemented "handle follow requests reject"))

(* * Route *)
let route = 
  Dream.scope "/follow-requests" [] [
    Dream.get "" @@ Error_display.handle_error_html (handle_follow_requests_get);
    Dream.post "/:id/accept" @@ Error_display.handle_error_json handle_follow_requests_accept;
    Dream.post "/:id/reject" @@ Error_display.handle_error_json handle_follow_requests_reject;
  ]

