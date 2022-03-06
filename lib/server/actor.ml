open Containers

let with_user req then_ =
  let (let+) x f = Common.request_bind req x f in
  let username = Dream.param req "username" in
  let+ user = Dream.sql req (Database.LocalUser.lookup_user ~username) in
  match user with
  | None -> Dream.respond ~status:`Not_Found "Not found"
  | Some user -> then_ user

let (let+) x f = x f
let (let*) x f = Lwt.bind x f
let (let*!) x f = Lwt_result.bind x f



let handle_actor_get config req =
  let+ current_user = Common.with_current_user req in
  let+ user = with_user req in
  let content_type = Dream.header req "Accept"
                     |> Option.value ~default:(Activitypub.ContentType.html) in
  match Activitypub.ContentType.of_string content_type with
  | None -> Dream.respond ~status:`Not_Acceptable "{}"
  | Some `HTML ->
    Dream.html (Html.Profile.build current_user user req)
  | Some `JSON ->
    Dream.respond
      ~headers:[("Content-Type", Activitypub.ContentType.activity_json)]
      (Yojson.Safe.to_string (Activitypub.LocalUser.of_local_user config user))

let handle_inbox_get req =
  Dream.log "GET to %s/inbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

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

let lookup_request url : (X509.Public_key.t, 'a) result Lwt.t =
  let*! (_resp, body) =
    try
      let* pair =
        Cohttp_lwt_unix.Client.get
          ~headers:(Cohttp.Header.of_list ["Accept", Activitypub.ContentType.activity_json])
          (Uri.of_string url) in
      Lwt_result.return pair
    with exn -> Lwt.return (Result.of_exn exn) in
  let* body = Cohttp_lwt.Body.to_string body in
  let*! body_json = Lwt.return @@ try Ok (Yojson.Safe.from_string body) with exn -> Result.of_exn exn in
  let+ public_key = member_safe __LINE__ "publicKey" body_json in
  let+ public_key_pem = member_safe __LINE__ "publicKeyPem" public_key in
  let+ public_key_pem = string_safe __LINE__ public_key_pem in
  let pub_key = X509.Public_key.decode_pem (Cstruct.of_string public_key_pem)
              |> Result.map_err (fun (`Msg err) -> err) in
  Lwt.return pub_key

let handle_inbox_post req =
  Dream.log "POST to %s/inbox" (Dream.param req "username");
  let* verification = Auth.verify_request lookup_request req in
  begin match verification with
  | Ok status -> Dream.log "verification status was %b" status
  | Error e -> Dream.log "error while verifying: %s" e
  end;
  let* body = Dream.body req in
  Dream.log "DATA: %s" body;
  Dream.respond ~status:`OK ""


let handle_outbox_get req =
  Dream.log "GET %s/outbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

let handle_outbox_post req =
  Dream.log "POST %s/outbox" (Dream.param req "username");
  let* body = Dream.body req in
  Dream.log "DATA: %s" body;
  Dream.respond ~status:`OK ""



let route config = 
    Dream.scope "/users" [] [
      Dream.get "/:username" (handle_actor_get config);
      Dream.get "/:username/inbox" handle_inbox_get;
      Dream.post ":username/inbox" handle_inbox_post;
      Dream.get "/:username/outbox" handle_outbox_get;
      Dream.post "/:username/outbox" handle_outbox_post;

    ]
