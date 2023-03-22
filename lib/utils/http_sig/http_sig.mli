open Containers

module StringMap : module type of Map.Make (String)


val parse_signature : string -> string StringMap.t
val verify: signed_string:string -> signature:string -> X509.Public_key.t -> bool

val verify_request:
  resolve_public_key:(string -> (X509.Public_key.t, 'a) Lwt_result.t) ->
  Dream.request -> (bool, 'a) result Lwt.t

val build_signed_string:
  signed_headers:string ->
  meth:string ->
  path:string -> headers:string StringMap.t -> body_digest:string -> string

val build_signed_headers:
  priv_key:X509.Private_key.t ->
  key_id:string ->
  headers:string StringMap.t ->
  ?body_str:string ->
  current_time:Ptime.t -> method_:string -> uri:Uri.t -> unit -> (string * string) list

val sign_headers:
  priv_key:X509.Private_key.t ->
  key_id:string ->
  ?body:Cohttp_lwt.Body.t ->
  headers:Cohttp.Header.t ->
  uri:Uri.t -> method_:Cohttp.Code.meth ->
  unit -> Cohttp.Header.t Lwt.t
