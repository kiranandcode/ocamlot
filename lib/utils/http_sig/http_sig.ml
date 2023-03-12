open Containers

module StringMap = Map.Make(String)
let log = Logging.add_logger "utils.http-sig"


(* from IRC  *)
let encrypt (privkey: X509.Private_key.t) str =
  Base64.encode (X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1
                   privkey
                   (`Message (Cstruct.of_string str))
                 |> Result.get_exn
                 |> Cstruct.to_string)

let time_now () =
  CalendarLib.Calendar.now ()
  |> CalendarLib.Calendar.to_unixfloat
  |> Ptime.of_float_s
  |> Option.get_exn_or "invalid date"  

let req_headers headers  =
  Cohttp.Header.to_list headers
  |> StringMap.of_list

let body_digest body =
  Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string body)
  |> Cstruct.to_string 
  |> fun hash -> "SHA-256=" ^ Base64.encode_string hash

(* constructs a signed string *)
let build_signed_string ~signed_headers ~meth ~path ~headers ~body_digest =
  (* (request-target) user-agent host date digest content-type *)
  String.split_on_char ' ' signed_headers
  |> List.map (function
    | "(request-target)" ->
      "(request-target): " ^ String.lowercase_ascii meth ^ " " ^ path
    | "digest" -> "digest: " ^ body_digest
    | header -> header ^ ": " ^ (StringMap.find_opt header headers |> Option.value ~default:"")
  )
  |> String.concat "\n"

let drop_quotes str = String.sub str 1 (String.length str - 2)

let split_equals str =
  match String.index_opt str '=' with
  | Some ind ->
    let key = String.sub str 0 ind in
    let data = String.sub str (ind + 1) (String.length str - ind - 1) in
    Some (key,data)
  | _ -> None

let parse_signature signature =
  String.split_on_char ',' signature
  |> List.filter_map split_equals
  |> List.map (Pair.map_snd drop_quotes)
  |> StringMap.of_list

let verify ~signed_string ~signature pubkey =
  let result =  X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1
    ~signature:(Cstruct.of_string signature)
    pubkey (`Message (Cstruct.of_string signed_string)) in
  match result with
  | Ok () -> true
  | Error `Msg e ->
    Dream.log "error while verifying: %s\n\nsigned_string is:%s\n\nsignature is:%s\n" e signed_string signature;
    false

let verify_request ~resolve_public_key (req: Dream.request) =
  let (let+) x f = match x with None -> Lwt.return (Ok false) | Some v -> f v in
  let (let*) x f = Lwt_result.bind x f in
  let (let@) x f = Lwt.bind x f in
  log.debug (fun f -> f "call to verify_request");
  let meth = Dream.method_ req |> Dream.method_to_string |> String.lowercase_ascii in
  let path = Dream.target req in
  let headers =
    Dream.all_headers req
    |> List.map (Pair.map_fst String.lowercase_ascii)
    |> StringMap.of_list in
  let+ signature = Dream.header req "Signature" in
  let signed_headers = parse_signature signature in
  log.debug (fun f -> f "reached verify_request:%d" __LINE__);
  (* 1. build signed string *)
  let@ body = Dream.body req in
  let body_digest = body_digest body in
  (* signed headers *)
  let+ headers_in_signed_string = StringMap.find_opt "headers" signed_headers in
  (* signed string *)
  let signed_string =
    build_signed_string ~signed_headers:headers_in_signed_string
      ~meth ~path ~headers ~body_digest in
  log.debug (fun f -> f "reached verify_request:%d" __LINE__);
  (* 2. retrieve signature *)
  let+ signature = StringMap.find_opt "signature" signed_headers in
  let+ signature = Base64.decode signature |> Result.to_opt in
  log.debug (fun f -> f "reached verify_request:%d" __LINE__);
  (* 3. retrieve public key *)
  let+ key_id = StringMap.find_opt "keyId" signed_headers in
  let* public_key = resolve_public_key key_id in
  log.debug (fun f -> f "reached verify_request:%d" __LINE__);
  (* verify signature against signed string with public key *)
  Lwt_result.return @@ verify ~signed_string ~signature public_key

let build_signed_headers ~priv_key ~key_id
      ~headers ~body_str ~current_time
      ~method_ ~uri =
  let signed_headers = "(request-target) content-length host date digest" in
  let body_str_len = String.length body_str |> Int.to_string in
  let body_digest = body_digest body_str in
  let date = Http_date.to_utc_string current_time in
  let host = uri
             |> Uri.host
             |> Option.get_exn_or "no host for request" in

  let signature_string =
    let to_be_signed =
      build_signed_string
        ~signed_headers
        ~meth:(method_ |> String.lowercase_ascii)
        ~path:(Uri.path uri)
        ~headers:(StringMap.add "content-length" body_str_len @@
                  StringMap.add "date" date @@
                  StringMap.add "host" host @@
                  headers)
        ~body_digest in

    let signed_string = encrypt priv_key to_be_signed |> Result.get_exn in
    Printf.sprintf {|keyId="%s",algorithm="rsa-sha256",headers="%s",signature="%s"|}
      key_id signed_headers signed_string in
  List.fold_left (fun map (k,v) -> StringMap.add k v map) headers
    ["Digest", body_digest;
     "Date", date;
     "Host", host;
     "Signature", signature_string;
     "Content-Length", body_str_len ]
  |> StringMap.to_list

let sign_headers ~priv_key ~key_id
      ~(body: Cohttp_lwt.Body.t)
      ~(headers: Cohttp.Header.t) ~uri ~method_ =
  let (let@) x f = Lwt.bind x f in

  let@ body_str = Cohttp_lwt.Body.to_string body in
  let current_time = time_now () in

  let headers =
    List.fold_left
      (fun header (key,vl) -> Cohttp.Header.add header key vl) headers
    (build_signed_headers ~priv_key ~key_id
       ~headers:(req_headers headers) ~body_str ~current_time
       ~method_:(Cohttp.Code.string_of_method method_) ~uri) in
  Lwt.return headers
