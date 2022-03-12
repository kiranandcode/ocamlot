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
      Activitypub.Decode.(follow)
      body in
  match follow with
  | Ok (follow_obj: Activitypub.Types.follow) ->
    Dream.log "received a follow object.";
    Dream.log "follow id: %s" follow_obj.id;
    Dream.log "follow actor: %s" follow_obj.actor;
    Dream.log "follow to: %s" @@ String.concat ", " follow_obj.to_;
    Dream.log "follow ccd: %s" @@ String.concat ", " follow_obj.cc;
    Dream.log "follow object?: %s" follow_obj.object_;
    Dream.respond ~status:`OK ""
  | Error e ->
    Dream.error (fun log -> log ~request:req "error while decoding follow request: %a"
                              Decoders_yojson.Safe.Decode.pp_error e
                );
    Dream.respond ~status:`Not_Acceptable ""

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
