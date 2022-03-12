open Containers
open Common
let with_user req then_ =
  let load_user req username = Dream.sql req (Database.LocalUser.lookup_user ~username) in
  with_param "username" load_user req ~then_ ~else_:(not_found ~msg:"User not found")

let handle_actor_get config req =
  let> user = with_user req in
  let content_type = Dream.header req "Accept"
                     |> Option.value ~default:(Activitypub.Constants.ContentType.html) in
  match Activitypub.Constants.ContentType.of_string content_type with
  | None -> Dream.respond ~status:`Not_Acceptable "{}"
  | Some `HTML ->
    let> current_user = Common.with_current_user req in
    Dream.html (Html.Profile.build current_user user req)
  | Some `JSON ->
    activity_json
      (user
       |> Database.Interface.LocalUser.convert_to config
       |> Activitypub.Encode.person)

let handle_inbox_get req =
  Dream.log "GET to %s/inbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

let enforce_is_true vl kont =
  if vl
  then kont ()
  else Dream.respond ~status:`Not_Acceptable {|{"error": "invalid request"}|}

let handle_inbox_post req =
  Dream.log "POST to %s/inbox" (Dream.param req "username");
  let> request_is_verified =
    Http_sig.verify_request ~resolve_public_key:Resolver.lookup_request req
    |> or_errorP ~req in
  let> () = request_is_verified |> holds_or ~else_:not_acceptable in
  let+ body = Dream.body req in
  Dream.log "DATA: %s" body;
  let follow =
    Decoders_yojson.Safe.Decode.decode_string
      Activitypub.Decode.(obj) body in
  match follow with
  | Error e ->
    Dream.error (fun log ->
      log ~request:req "error while decoding request: %a"
        Decoders_yojson.Safe.Decode.pp_error e);
    Dream.respond ~status:`Not_Acceptable ""
  | Ok obj ->
    Dream.log "recieved an obj:\n%a"
      Activitypub.Types.pp_obj obj;
    match obj with
    | `Create _c ->
      Dream.log "received a create object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Follow (f: Activitypub.Types.follow) ->
      Dream.log "follow: %a" Activitypub.Types.pp_follow f;
      Dream.log "received a follow object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Announce _ ->
      Dream.log "received an announce object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Block _ ->
      Dream.log "received a block object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Note _ ->
      Dream.log "received a note object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Person _ ->
      Dream.log "received a person object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Undo _ ->
      Dream.log "received an undo object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Delete _ ->
      Dream.log "received a delete object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Accept _ ->
      Dream.log "received an accept object!";
      Dream.respond ~status:`Not_Implemented "lol"
    | `Like _ ->
      Dream.log "received a like object!";
      Dream.respond ~status:`Not_Implemented "lol"


let handle_outbox_get req =
  Dream.log "GET %s/outbox" (Dream.param req "username");
  Dream.respond ~status:`OK ""

let handle_outbox_post req =
  Dream.log "POST %s/outbox" (Dream.param req "username");
  let+ body = Dream.body req in
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
