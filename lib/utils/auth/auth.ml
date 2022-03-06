open Containers
module StringMap = Map.Make(String)

(* from IRC  *)
let encrypt (privkey: X509.Private_key.t) str =
  Base64.encode (X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1
                   privkey
                   (`Message (Cstruct.of_string str))
                 |> Result.get_exn
                 |> Cstruct.to_string)

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

let parse_signature signature =
  String.split_on_char ',' signature
  |> List.filter_map (fun entry ->
    match String.split_on_char '=' entry with
    | [k;v] -> Some (k,drop_quotes v)
    | _ -> None
  )
  |> StringMap.of_list

let verify signed_string signature pubkey =
  let result =  X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1
    ~signature:(Cstruct.of_string signature)
    pubkey
    (`Digest (Cstruct.of_string signed_string)) in
  match result with
  | Ok () -> Lwt_result.return true
  | Error _ -> Lwt_result.return false

let verify_request resolve_public_key (req: Dream.request) =
  let (let+) x f = match x with None -> Lwt.return (Ok false) | Some v -> f v in
  let (let*) x f = Lwt_result.bind x f in
  let (let@) x f = Lwt.bind x f in
  let meth = Dream.method_ req |> Dream.method_to_string |> String.lowercase_ascii in
  let[@warning "-26-depracated"] path =
    Dream.path req |> String.concat "/" |> fun result -> "/" ^ result in
  Dream.log "verifying request";
  let headers = Dream.all_headers req |> StringMap.of_list in
  Dream.log "headers is %a" (StringMap.pp ~pp_arrow:(fun fmt () -> Format.pp_print_string fmt " -> ") String.pp String.pp) headers;
  let+ signature = Dream.header req "Signature" in
  Dream.log "signature is %s" signature;
  let hsig = parse_signature signature in
  Dream.log "hsig is %a" (StringMap.pp ~pp_arrow:(fun fmt () -> Format.pp_print_string fmt " -> ") String.pp String.pp) hsig;

  (* 1. build signed string *)
  let@ body = Dream.body req in
  Dream.log "body is %s" body;
  let body_digest = body_digest body in
  Dream.log "body digest is %s" body_digest;
  (* signed headers *)
  let+ signed_headers = StringMap.find_opt "headers" hsig in
  Dream.log "signed headers is %s" signed_headers;
  (* signed string *)
  let signed_string = build_signed_string ~signed_headers ~meth ~path ~headers ~body_digest in

  Dream.log "signed string is:\n%s" signed_string;

  (* 2. retrieve signature *)
  let+ signature = StringMap.find_opt "signature" hsig in
  Dream.log "signature was:\n%s" signed_string;
  let+ signature = Base64.decode signature |> Result.to_opt in

  Dream.log "decoded signature was:\n%s" signed_string;

  (* 3. retrieve public key *)
  let+ key_id = StringMap.find_opt "keyId" hsig in
  let* public_key = resolve_public_key key_id in

  Dream.log "was able to decode the public key";

  (* verify signature against signed string with public key *)
  verify signed_string signature public_key

let sign_headers ~priv_key ~key_id
      ~(body: Cohttp_lwt.Body.t)
      ~(headers: Cohttp.Header.t) ~uri ~method_ =
  let (let@) x f = Lwt.bind x f in

  let signed_headers = "(request-target) user-agent host date digest content-type" in

  let add_header key vl headers = Cohttp.Header.add headers key vl in

  let@ body_digest = 
    let@ body_str = Cohttp_lwt.Body.to_string body in
    Lwt.return (body_digest body_str) in

  let date = Http_date.to_utc_string (CalendarLib.Calendar.now ()
                                      |> CalendarLib.Calendar.to_unixfloat
                                      |> Ptime.of_float_s
                                      |> Option.get_exn_or "invalid date") in
  let host = uri
             |> Uri.host
             |> Option.get_exn_or "no host for request" in


  let signature_string =
    let to_be_signed =
      build_signed_string
        ~signed_headers
        ~meth:(Cohttp.Code.string_of_method method_ |> String.lowercase_ascii)
        ~path:(Uri.path uri)
        ~headers:(req_headers headers)
        ~body_digest in

    let signed_string = encrypt priv_key to_be_signed |> Result.get_exn in
    Printf.sprintf {|keyId="%s",algorithm="rsa-sha256",headers="%s",signature="%s"|}
      key_id signed_headers signed_string in

  let headers =
    headers
    |> add_header "Digest" body_digest
    |> add_header "Date" date
    |> add_header "Host" host
    |> add_header "Signature" signature_string in

  Lwt.return headers
