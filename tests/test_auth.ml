module T = Testing_utils.Make (struct let name = "auth" end) ;;
open Containers

let pub_key = "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4rUt9vwGzXpdSak2NTVi\noQErRMoDYxzF8yWpMjgnETLqYFVft+7utpDsXJApJ6hWB8IZP4t0MIOEEWrWl0V0\nVoGu649AkSxf+M4L8iqRP8qYUTZ+ghl3TYOgDZuUknPFMLtVWNtDsqpLkRrJBBBx\n4md4tdVZMffmAmENuPhuCgHBmGKTNL8nRN6A1R5YE3R7Zn5ekm49PShvMgd3YRXM\n6l1pRrYhKDAUtuScNi7DwZRX4ihyGxsmZSpGuUjjRLOSNMOp61Mq3oeTg11+AN0o\nUPF7B8BuF82GOIJFLQJb2rEsPF12cToaBD4GFVJ3M0IkHG6B+k2SOjP87hMPEPzs\nswIDAQAB\n-----END PUBLIC KEY-----\n\n"

let key = X509.Public_key.decode_pem (Cstruct.of_string pub_key) |> Result.get_exn
let signature = "gvO8MbE7frP8DYTt52w3AwPAO2B1LZ15+nvikcHO5rH+8QfNiKoxbyLQ8FH05X3WytkCDnlIM5Ek731iTZdA+3VTzv8aLZI7Osq6JOIa62QNp5CR8wCuAkU8du/Xn6rnmNDnrWKByOKiNnsRaYbLb82jQoXXBt2CBtNb7+AeQgsiWY6UXhKJN7/Efwj6mp1Ogf8tUQZVpd7kUBAOwGWmWhP0Q0CoCBOZj7kxX0fTC1r78VzpsZtl0X7RjiyTeA+NzcRNfo1DQ1MTmkeQneIk8Bp7qGqcH9PyycEmJIMs93xpkILcDgBWy7tWei2aop0GR0ZDoyHO2HuPdd9UdrOmhA=="
let signed_string = "(request-target): post /users/example/inbox
content-length: 393
date: Sun, 06 Mar 2022 11:19:18 GMT
digest: SHA-256=DelghzCYphHHh1k6zXwdc+gUNO9nhSTNOJFnC6qDm6E=
host: ocamlot.nfshost.com"


let decoded_signature = Base64.decode signature |> Result.get_exn

let verify signed_string signature pubkey =
  let result =  X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1
    ~signature:(Cstruct.of_string signature)
    pubkey
    (`Message (Cstruct.of_string signed_string)) in
  match result with
  | Ok () -> Lwt_result.return true
  | Error `Msg e -> failwith e
;;

T.add_test "parsing signatures works correctly" @@  fun () ->
let signature = {|keyId="https://ocamlot.xyz/users/atestaccount#main-key",algorithm="rsa-sha256",headers="(request-target) content-length date digest host",signature="Rw8WUQlm1QFiPUGvoORDJO3w9urA91M94VnAcZTIu3edfVbIuM1Eac+EdMIUP4+Qqz3HNVE5PCHvn5XcDtU337dxP0aZnkvoHxi8cssDzUkh3AIyuLQglzYxo36Q4PG1guz2IfvuJK5xtxLn3ELbRP2KkARUmm4uJYhFXoe7BBBCT9HezvfCOiHSDzLZr4ZGuNsW1x31twN1YEq/+Qzf62do87bnSg3IvZXHDkUDojw7zpgOEZVfvCDZTwPPTzMP/B1lRbUfuAHqK1JrAOmgV2s4t7VmYE+pkg+s9E9kmfYeJGcGeUDvKgYZxOfbWMepGZLMQFXjioeq6+7ml0Kckg=="|} in
let headers = Auth.parse_signature signature in
let is_present key = Auth.StringMap.mem key headers in
Alcotest.(check bool) "all expected keys are present" true @@ List.for_all is_present [
  "keyId"; "algorithm"; "headers"; "signature"
]
;;



let () = T.run ()
