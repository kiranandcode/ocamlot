open Containers
open Common


let req_post ~headers url body =
  let body = Cohttp_lwt.Body.of_string body in
  try
    Cohttp_lwt_unix.Client.post ~headers ~body url >> Result.return
  with exn ->
    Lwt.return (Result.of_exn exn)

let req ~headers url =
  Cohttp_lwt_unix.Debug.activate_debug ();
  try
    Cohttp_lwt_unix.Client.get
      ~headers:(Cohttp.Header.of_list headers)
      url
    >> Result.return
  with exn -> Lwt.return (Result.of_exn exn)

let signed_req f (key_id, priv_key) uri body_str =
  let current_time = Ptime_clock.now () in
  let headers =
    Http_sig.build_signed_headers
      ~current_time ~method_:"POST" ~body_str
      ~headers:(Http_sig.StringMap.of_list [
        "Content-Type", APConstants.ContentType.ld_json_activity_streams
      ]) ~key_id ~priv_key ~uri
    |> Cohttp.Header.of_list in
  f ~headers uri body_str

let signed_post key uri body =
  signed_req req_post key uri body

let activity_req ?(headers=[]) url =
  let activity_header =
    ("Accept", APConstants.ContentType.activity_json) in
  req ~headers:(activity_header :: headers) url

let json_rd_req ?(headers=[]) url =
  let json_rd_header =
    ("Accept", APConstants.Webfinger.json_rd) in
  req ~headers:(json_rd_header :: headers) url
