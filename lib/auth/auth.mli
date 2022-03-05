
val verify_request:
  (string -> (X509.Public_key.t, 'a) Lwt_result.t) ->
  Dream.request -> (bool, 'a) result Lwt.t

val sign_headers:
  priv_key:X509.Private_key.t ->
  key_id:string ->
  body:Cohttp_lwt.Body.t ->
  headers:Cohttp.Header.t ->
  uri:Uri.t -> method_:Cohttp.Code.meth ->
  Cohttp.Header.t Lwt.t
