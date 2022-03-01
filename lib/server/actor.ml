let handle_actor_get req =
  Dream.log "got username %s" (Dream.param req "username");
  Dream.html ((Dream.param req "username") ^ "'s inbox")

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


let route = 
    Dream.scope "/user" [] [
      Dream.get "/:username" handle_actor_get;
      Dream.get "/:username/inbox" handle_inbox_get;
      Dream.post ":username/inbox" handle_inbox_post;
      Dream.get "/:username/outbox" handle_outbox_get;
      Dream.post "/:username/outbox" handle_outbox_post;

    ]
