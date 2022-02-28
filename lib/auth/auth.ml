(* from IRC  *)
let encrypt (privkey: X509.Private_key.t) str =
  Base64.encode (X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1  privkey (`Message (Cstruct.of_string str)) |> Result.get_ok |> Cstruct.to_string)


let update_headers (_r: Cohttp.Request.t) = ()


