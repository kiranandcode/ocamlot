
let with_user req then_ =
  let (let+) x f = Common.request_bind req x f in
  let username = Dream.param req "username" in
  let+ user = Dream.sql req (Database.LocalUser.lookup_user ~username) in
  match user with
  | None -> Dream.respond ~status:`Not_Found "Not found"
  | Some user -> then_ user

let (let+) x f = x f
let (let*) x f = Lwt.bind x f



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


let handle_inbox_post req =
  Dream.log "POST to %s/inbox" (Dream.param req "username");
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
