open Containers
open Common


let req_post ~headers url body =
  let body = Cohttp_lwt.Body.of_string body in
  try
    Cohttp_lwt_unix.Client.post ~headers ~body url >> Result.return
  with exn ->
    Lwt.return (Result.of_exn exn)

let req ~headers url =
  try
    Cohttp_lwt_unix.Client.get
      ~headers
      url
    >> Result.return
  with exn -> Lwt.return (Result.of_exn exn)

let signed_post (key_id, priv_key) uri body_str =
  let current_time = Ptime_clock.now () in
  let headers =
    Http_sig.build_signed_headers
      ~current_time ~method_:"POST" ~body_str
      ~headers:(Http_sig.StringMap.of_list [
        "Content-Type", APConstants.ContentType.ld_json_activity_streams
      ]) ~key_id ~priv_key ~uri ()
    |> Cohttp.Header.of_list in
  req_post ~headers uri body_str

let signed_get ?accept (key_id, priv_key) uri =
  let current_time = Ptime_clock.now () in
  let headers =
    Http_sig.build_signed_headers
      ~current_time ~method_:"GET" 
      ~headers:(Http_sig.StringMap.of_list [
        "Accept", Option.value accept ~default:APConstants.ContentType.ld_json_activity_streams
      ]) ~key_id ~priv_key ~uri ()
    |> Cohttp.Header.of_list in
  req ~headers uri

let signed_activity_req key url =
  signed_get ~accept:APConstants.ContentType.activity_json key url

let activity_req ?(headers=[]) url =
  let activity_header =
    ("Accept", APConstants.ContentType.activity_json) in
  req ~headers:(Cohttp.Header.of_list (activity_header :: headers)) url

let json_rd_req ?(headers=[]) url =
  let json_rd_header =
    ("Accept", APConstants.Webfinger.json_rd) in
  req ~headers:(Cohttp.Header.of_list (json_rd_header :: headers)) url
