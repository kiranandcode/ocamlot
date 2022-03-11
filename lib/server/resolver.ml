open Containers
open Common


let string_safe line elt kont =
  match elt with
  | `String str -> kont str
  | _ ->
    Lwt.return (Error (Int.to_string line ^ "could not convert " ^ Yojson.Safe.to_string elt ^ " into a string"))

let member_safe line name elt kont =
  match elt with
  | `Assoc ls -> begin match List.assoc_opt ~eq:String.(=) name ls with
    | Some res -> kont res
    | None ->
      Lwt.return (Error (Int.to_string line ^ "could not deref " ^ name ^ " from: " ^ Yojson.Safe.to_string elt))      
    end
  | _ -> Lwt.return (Error (Int.to_string line ^ "could not deref " ^ name ^ " from: " ^ Yojson.Safe.to_string elt))

let lookup_request url =
  let (let+!) x f = Lwt_result.bind x f in
  let+! (_resp, body) =
      try
        let+ pair =
          Cohttp_lwt_unix.Client.get
            (* NOTE: Not obvious, but you need to specify accept headers, else pleroma will return html *)
            ~headers:(Cohttp.Header.of_list ["Accept", Activitypub.Constants.ContentType.activity_json])
            (Uri.of_string url) in
        Lwt_result.return pair
      with exn -> Lwt.return (Result.of_exn exn) in
  let+! actor = Cohttp_lwt.Body.to_string body
              |> Lwt.map Activitypub.Decode.(decode_string person) in
  let pub_key =
    actor.public_key.pem
    |> Cstruct.of_string
    |> X509.Public_key.decode_pem
    |> Result.map_err (fun (`Msg err) -> err) in
  Lwt.return pub_key

