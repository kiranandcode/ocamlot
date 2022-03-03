
let with_user req then_ =
  let (let+) x f = Common.request_bind req x f in
  let username = Dream.param req "username" in
  let+ user = Dream.sql req (Database.User.lookup_user ~username) in
  match user with
  | None -> Dream.respond ~status:`Not_Found "Not found"
  | Some user -> then_ user


let handle_actor_get config req =
  with_user req begin fun user ->
    Dream.json (Yojson.Safe.to_string (Api.User.to_json config user))
  end


let handle_inbox_get req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let handle_inbox_post req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let handle_outbox_get req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

let handle_outbox_post req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")


let route config = 
    Dream.scope "/user" [] [
      Dream.get "/:username" (handle_actor_get config);
      Dream.get "/:username/inbox" handle_inbox_get;
      Dream.post ":username/inbox" handle_inbox_post;
      Dream.get "/:username/outbox" handle_outbox_get;
      Dream.post "/:username/outbox" handle_outbox_post;

    ]
