module T = Testing_utils.Lwt.Make (struct let name = "auth" end) ;;
open Containers
let (let+) = Lwt.bind

let priv_key = {|-----BEGIN PRIVATE KEY-----
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
OILvfb2+s4BZk1BM5JHAXjj52N212w==
-----END PRIVATE KEY-----|}

let signed_string = {|(request-target): post /users/example/inbox
content-length: 393
date: Sun, 06 Mar 2022 13:05:44 GMT
digest: SHA-256=Eg24tq9YA9SmHSex1hN/QZvkj2Iq4jOtRtMPzG/UkyA=
host: ocamlot.nfshost.com|}

let signature = {|hndf0UD2a57L4ly1O6LxEp56YV/Ewha0iJlEOE4l/dJZcB6cQwHw2jF4bX4FRLIERHIJ8iHKRZjMg7DVYpZ7vvqZdPpkuEVr6FDdTDRBejRIFbfgeK7sXfmGOgY92xwl0DRxG/TjD21KvpOm0iswOq3zheS0Dm5XfBeu0TiVcP3PVuFB+NeoYpm1py+G9fqAqD0eZp5oc8Bimiq8NoSYZehF9IvxXLRoDKTRhOdIX2mMBRFzANge1rguW+t7dVHeemzGJsO588hPjP+vUaV7unwbz6oHzNce4MryuIJMMQ5AU2hO7pglOEMUp9lG/kfAnUA/ARIZTsCP59yx3u2GLg==|}

let pubkey_pem = {|-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAuxAAzCzYWw6cXT8p6hmO
Bjb1sbrA1SqslohuXDhHXPsNRkydg1P/8U+PMXyB7E9N1K57uqcq/D+KJU5njbRp
z7m8yB3zZNvcQc7rGx7JtEa7ym/zbcCtZAhE9H6G8muCIBv5Rhy02Q8SgafiQQsX
Q+NGubqTdUW8HwiRn4HfYNV9IxDqv0mvT2jkECJTjm38hPjSGbBj0N1FtCPBUSU4
88kZgITCRKrSCPH5ZAIRgFesThwTyauiGii1h3EXWprzgsRd2yRBkf7XT+JOEUlt
Wd7DO70AJKyHiP2cuG3MVP7H71+G5l/xXm4WR46FA6NRI6ChWEvsyuG/aPrqjAqa
8QIDAQAB
-----END PUBLIC KEY-----|}

let follow_obj = {|{"@context":["https://www.w3.org/ns/activitystreams","https://ocamlot.xyz/schemas/litepub-0.1.jsonld",{"@language":"und"}],"actor":"https://ocamlot.xyz/users/multi-mention","cc":["https://ocamlot.xyz/users/multi-mention/followers"],"context":"https://ocamlot.xyz/contexts/79abd604-8b1b-49e7-8608-234d97a9e3df","context_id":29,"directMessage":false,"id":"https://ocamlot.xyz/activities/7e12aac7-643d-421d-9b4a-7360996f34fc","object":{"actor":"https://ocamlot.xyz/users/multi-mention","attachment":[],"attributedTo":"https://ocamlot.xyz/users/multi-mention","cc":["https://ocamlot.xyz/users/multi-mention/followers"],"content":"<span class=\"h-card\"><a class=\"u-url mention\" data-user=\"AH8Iw1uMJ5QFtk4ycy\" href=\"https://ocamlot.nfshost.com/users/example\" rel=\"ugc\">@<span>example</span></a></span> hey there","context":"https://ocamlot.xyz/contexts/79abd604-8b1b-49e7-8608-234d97a9e3df","conversation":"https://ocamlot.xyz/contexts/79abd604-8b1b-49e7-8608-234d97a9e3df","id":"https://ocamlot.xyz/objects/998b7754-bfc9-4f44-b2f1-739b37298d57","published":"2022-03-06T15:16:26.823295Z","sensitive":null,"source":"@example@ocamlot.nfshost.com hey there","summary":"","tag":[{"href":"https://ocamlot.nfshost.com/users/example","name":"@example@ocamlot.nfshost.com","type":"Mention"}],"to":["https://www.w3.org/ns/activitystreams#Public","https://ocamlot.nfshost.com/users/example"],"type":"Note"},"published":"2022-03-06T15:16:26.821690Z","to":["https://www.w3.org/ns/activitystreams#Public","https://ocamlot.nfshost.com/users/example"],"type":"Create"}|}
;;

T.add_test "parsing signatures works correctly" @@  fun () ->
let signature = {|keyId="https://ocamlot.xyz/users/atestaccount#main-key",algorithm="rsa-sha256",headers="(request-target) content-length date digest host",signature="Rw8WUQlm1QFiPUGvoORDJO3w9urA91M94VnAcZTIu3edfVbIuM1Eac+EdMIUP4+Qqz3HNVE5PCHvn5XcDtU337dxP0aZnkvoHxi8cssDzUkh3AIyuLQglzYxo36Q4PG1guz2IfvuJK5xtxLn3ELbRP2KkARUmm4uJYhFXoe7BBBCT9HezvfCOiHSDzLZr4ZGuNsW1x31twN1YEq/+Qzf62do87bnSg3IvZXHDkUDojw7zpgOEZVfvCDZTwPPTzMP/B1lRbUfuAHqK1JrAOmgV2s4t7VmYE+pkg+s9E9kmfYeJGcGeUDvKgYZxOfbWMepGZLMQFXjioeq6+7ml0Kckg=="|} in
let headers = Auth.parse_signature signature in
let is_present key = Auth.StringMap.mem key headers in
Lwt.return @@
Alcotest.(check bool) "all expected keys are present" true @@ List.for_all is_present [
  "keyId"; "algorithm"; "headers"; "signature"
]
;;

T.add_test "verifing signatures works correctly" @@ fun () ->
let pubkey = X509.Public_key.decode_pem (Cstruct.of_string pubkey_pem)
             |> Result.get_exn in
let signature = Base64.decode_exn signature in
Lwt.return @@
Alcotest.(check bool) "signature is verified correctly" true
  (Auth.verify ~signature ~signed_string pubkey)
;;

T.add_test "verifing signatures fails correctly" @@ fun () ->
(* invalid signed string *)
let signed_string = {|(request-target): post /users/example/inbox
content-length: 394
date: Sun, 06 Mar 2022 13:05:44 GMT
digest: SHA-256=Eg24tq9YA9SmHSex1hN/QZvkj2Iq4jOtRtMPzG/UkyA=
host: ocamlot.nfshost.com|} in

let pubkey = X509.Public_key.decode_pem (Cstruct.of_string pubkey_pem)
             |> Result.get_exn in
let signature = Base64.decode_exn signature in
Lwt.return @@
Alcotest.(check bool) "signature is rejected correctly" false
  (Auth.verify ~signature ~signed_string pubkey)
;;

T.add_test "build signed string works correctly" @@ fun () ->

let signed_string' =
  Auth.build_signed_string
    ~signed_headers:"(request-target) content-length date digest host"
    ~meth:"post"
    ~path:"/users/example/inbox"
    ~body_digest:"SHA-256=Eg24tq9YA9SmHSex1hN/QZvkj2Iq4jOtRtMPzG/UkyA="
    ~headers:(Auth.StringMap.of_list [
      "content-length", "393";
      "date", "Sun, 06 Mar 2022 13:05:44 GMT";
      "host", "ocamlot.nfshost.com"
    ]) in

Lwt.return @@
Alcotest.(check string) "correct signed string constructed"
  signed_string signed_string'
;;


(* End-to-end test taken from live follow request from pleroma *)
T.add_test "verifying a full pleroma request works" @@ fun () -> 
let pubkey_pem = {|-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzIifP9k43yRgEXlUkB/y
oPOFkkY12dpMVvyTQQ3u1KYtUVGgVTgna92kJ85awvP8ZWDlVcVWjvLSWseRR/Nv
56O+0DuDiBwCBsQHFxA76+TES1G/tygE42FzJNtg1jgIe9YEPOZPKhVn3fiSEbhY
DqM4CO0/B5sfkoO0tYWpkUsWdG6Smy5nw9BD0vTEfnFbpCMYyOan2WCyxVuyTmil
R6t7VLJK3/THUq5PlYMuINLv+QRvcHVCVffTROACnc/7VVWu0wueJVQZl7msA367
tWa9+ktknP3xcYrJtgQLwgxpc0nouUli+SAjIjIViWIa79W5c/De/IfMbrZp6oZ+
LwIDAQAB
-----END PUBLIC KEY-----|} in
let req = Dream.request
            ~method_:`POST
            ~target:"/users/example/inbox"
            ~headers:[
              "Content-Length", "1556";
              "Content-Type", "application/activity+json";
              "Date", "Sun, 06 Mar 2022 15:16:26 GMT";
              "Digest", "SHA-256=TjDxftimoYfYco2r/MuuvqdISn4fV/TqWAuPDMguyJs=";
              "Host", "ocamlot.nfshost.com";
              "Signature", {|keyId="https://ocamlot.xyz/users/multi-mention#main-key",algorithm="rsa-sha256",headers="(request-target) content-length date digest host",signature="SiXAS6murhSy+/Nz8cKMeL77exJ824f9U1wEYfvZwvZ7iDL+I1LcgI+rxkNpeOsAs/OhTP3zTz05DJdA8TVNmQN66/Q7640Li/FAdfsaAXTXdRZ/ZHH92xq4lYVCYS7peY2U6oMaH0+NPrBXw8BVjUAYZtH8A94l1zndFej42K7vyaVlcas9Xk7abVseZTcGE2A6/eH1Y2A2spzG+mpcoilwxFmVSCXGeh5vk/mQxVGhQ2zN+yGubtGDNufGL86/WxDVdfxmT37hFM/WnJdhMR0t9aF0X0B8yA/Iql+U21cAr3qTE2Njv513hzsu7H3Lu+cT832cJkrJ026ZYPx6cg=="|};
              "User-Agent", "Pleroma 2.4.2; https://ocamlot.xyz <john@ocamlot.xyz>";              
            ] follow_obj in
let resolve_public_key url =
  Alcotest.(check string) "server calls resolve public key on key-id"
    "https://ocamlot.xyz/users/multi-mention#main-key" url;
  Lwt.return @@ X509.Public_key.decode_pem (Cstruct.of_string pubkey_pem) in
let+ request_response = Auth.verify_request ~resolve_public_key req in
Alcotest.(check bool) "response is some" true (Result.is_ok request_response);
Alcotest.(check bool) "response is true" true (Result.get_exn request_response);
Lwt.return_unit
;;

T.add_test "can create signed request that is can be decoded" @@ fun () ->
let date = "Sun, 06 Mar 2022 15:16:26 GMT" in
let priv_key = X509.Private_key.decode_pem (Cstruct.of_string priv_key) |> Result.get_exn in
let current_time = date |> Http_date.parse_date |> Option.get_exn_or "invalid assumptions" in
let key_id = "https://ocamlot.xyz/users/multi-mention#main-key" in
let headers = [
  "Content-Type", "application/activity+json";
  "Host", "ocamlot.nfshost.com";
  "Content-Length", Int.to_string @@ String.length follow_obj;
] |> Auth.StringMap.of_list in
let uri = "https://ocamlot.nfshost.com/users/example/inbox" |> Uri.of_string in
let method_ = `POST in
let headers =
  Auth.build_signed_headers
  ~priv_key ~key_id ~headers
  ~body_str:follow_obj ~current_time
  ~method_:(Dream.method_to_string method_) ~uri in
let req =  Dream.request ~method_ ~target:(Uri.path uri)
             ~headers follow_obj in
let resolve_public_key url =
  Alcotest.(check string) "server calls resolve public key on key-id"
    key_id url;
  Lwt.return @@ Ok (X509.Private_key.public priv_key) in
let+ request_response = Auth.verify_request ~resolve_public_key req in
Alcotest.(check bool) "response is some" true (Result.is_ok request_response);
Alcotest.(check bool) "response is true" true (Result.get_exn request_response);
Lwt.return_unit
;;

let () =
  Mirage_crypto_rng_unix.initialize ();
  T.run ()
 
