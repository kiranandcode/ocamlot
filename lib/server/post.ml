open Containers
open Common

let log = Logging.add_logger "web.post"
let limit = 10

let sql req f = Dream.sql req f |> Lwt_result.map_error (fun err -> `DatabaseError (Caqti_error.show err))

let parse_calendar s =
  let (let*) x f = Option.bind x f in
  let* s = Float.of_string_opt s in
  Ptime.of_float_s s


(* * Extracting data *)
let handle_post_get req =
  let public_id = Dream.param req "postid" in
  let* post =
    sql req (Database.Posts.lookup_by_public_id ~public_id) in
  let* post =
    post
    |> lift_opt ~else_:(fun () -> `ActivityNotFound "Could not find the requested post")
    |> return in
  let* can_access_post =
    if post.Database.Posts.is_public
    then return_ok true
    else
      let* current_user = current_user_link req in
      let* current_user =
        current_user
        |> lift_opt ~else_:(fun () -> `ActivityNotFound "Could not find the requested post")
        |> return in
      sql req (Database.Posts.is_visible_post ~by:current_user ~post:post.Database.Posts.id) in
  if can_access_post
  then 
    let* post_data = Extract.extract_post req post in
    let post_data = {
      post_data with
      reply_link=Some (Configuration.Url.write_reply_path post.url);
    } in
    let* headers,action = Navigation.build_navigation_bar req in
    let title =
      Option.value ~default:"(Untitled post)"
        post.Database.Posts.summary in
    let start_time =
      Dream.query req "offset-time"
      |> Option.flat_map parse_calendar
      |> Option.value ~default:(Ptime_clock.now ()) in
    let offset =
      Dream.query req "offset-start"
      |> Option.flat_map Int.of_string in
    let* related_posts, related_posts_count =
      let* current_user = current_user_link req in
      match current_user with
      | None -> return_ok ([], 0)
      | Some user ->
        let* posts =
          sql req (Database.Posts.collect_related_posts
                     ?offset ~limit ~start_time ~user:user ~post:post.id) in
        let* total_posts =
          sql req (Database.Posts.count_related_posts ~start_time ~user:user ~post:post.id) in
        let* post_data =
          Lwt_list.map_s (fun post -> Extract.extract_post req post) posts
          >> Result.flatten_l in
        return_ok (post_data, total_posts) in
    let navigation_panel =
      View.Components.render_pagination_numeric
        ~start:1 ~stop:(related_posts_count / limit + 1)
        (* ~current:(offset/limit) *)
        (fun ind ->
           Format.sprintf "/post/%s?offset-time=%f&offset-start=%d"
             public_id (Ptime.to_float_s start_time)
             ((ind - 1) * limit)
        ) () in
    tyxml @@ View.Page.render_page title @@ ([
        View.Header.render_header ?action headers;
        View.Post.render ~render_attachments:true post_data;
        View.Components.render_heading ~icon:"R" ~current:"Replies" ();
      ] @ (List.map (fun post -> View.Post.render post) related_posts) @ [navigation_panel])
  else
    Lwt_result.fail (`ActivityNotFound "Could not find the requested post")

let handle_post_toast req =
  let public_id = Dream.param req "postid" in
  let* current_user = current_user req in
  match current_user with
  | None -> redirect req "/feed"
  | Some user ->
    let* post =
      sql req (Database.Posts.lookup_by_public_id ~public_id) in
    let* post =
      post
      |> lift_opt ~else_:(fun () -> `ActivityNotFound "Could not find the requested post")
      |> return in
    Worker.send_task (LocalLike {user;post});
    let url =
      Dream.query req "redirect"
      |> Option.value ~default:("/post/" ^ public_id) in
    redirect req url

let handle_post_cheer req =
  let public_id = Dream.param req "postid" in
  let* current_user = current_user req in
  match current_user with
  | None -> redirect req "/feed"
  | Some user ->
    let* post =
      sql req (Database.Posts.lookup_by_public_id ~public_id) in
    let* post =
      post
      |> lift_opt ~else_:(fun () -> `ActivityNotFound "Could not find the requested post")
      |> return in
    Worker.send_task (LocalReboost {user;post});
    let url =
      Dream.query req "redirect"
      |> Option.value ~default:("/post/" ^ public_id) in
    redirect req url

let handle_get_remote req =
  let (let-!) x f = Lwt.bind x (function
      | Error err -> return (Error err)
      | Ok(None) -> redirect req "/feed"
      | Ok(Some x) -> f x) in
  let-! url = return_ok (Dream.query req "url") in
  let-! post = sql req (Database.Posts.lookup_by_url ~url) in
  let* can_access_post =
    if post.Database.Posts.is_public
    then return_ok true
    else
      let* current_user = current_user_link req in
      let* current_user =
        current_user
        |> lift_opt ~else_:(fun () -> `ActivityNotFound "Could not find the requested post")
        |> return in
      sql req (Database.Posts.is_visible_post ~by:current_user ~post:post.Database.Posts.id) in
  if can_access_post
  then
    let* headers,action = Navigation.build_navigation_bar req in
    let title =
      Option.value ~default:"(Untitled post)"
        post.Database.Posts.summary in

    let start_time =
      Dream.query req "offset-time"
      |> Option.flat_map parse_calendar
      |> Option.value ~default:(Ptime_clock.now ()) in
    let offset =
      Dream.query req "offset-start"
      |> Option.flat_map Int.of_string in
    let* related_posts, related_posts_count =
      let* current_user = current_user_link req in
      match current_user with
      | None -> return_ok ([], 0)
      | Some user ->
        let* posts =
          sql req (Database.Posts.collect_related_posts
                     ?offset ~limit ~start_time ~user:user ~post:post.id) in
        let* total_posts =
          sql req (Database.Posts.count_related_posts ~start_time ~user:user ~post:post.id) in
        let* post_data =
          Lwt_list.map_s (fun post -> Extract.extract_post req post) posts
          >> Result.flatten_l in
        return_ok (post_data, total_posts) in

    let navigation_panel =
      View.Components.render_pagination_numeric
        ~start:1 ~stop:(related_posts_count / limit + 1)
        (* ~current:(offset/limit) *)
        (fun ind ->
           Format.sprintf "/post/remote?url=%s&offset-time=%f&offset-start=%d"
             post.url (Ptime.to_float_s start_time)
             ((ind - 1) * limit)
        ) () in

    let* post_data = Extract.extract_post req post in
    let post_data = {
      post_data with
      self_link = Some (Uri.of_string url);
      reply_link=Some (Configuration.Url.write_reply_path url);
    } in
    tyxml @@ View.Page.render_page title @@ ([
      View.Header.render_header ?action headers;
      View.Post.render ~render_attachments:true post_data;
    ] @ (List.map (fun post -> View.Post.render post) related_posts) @ [navigation_panel])
  else redirect req "/feed"

let handle_post_remote req =
  let* current_user = current_user req in
  let url = Dream.query req "url" in
  let action = Dream.query req "action" in
  match current_user, url, action with
  | Some user, Some url, Some action when String.equal action "toast" || String.equal action "cheer"  ->
    log.debug (fun f -> f "received %s of remote post %s" action url);
    let* post = sql req (Database.Posts.lookup_by_url ~url) in
    let* post = post
                |> lift_opt ~else_:(fun () -> `ActivityNotFound "Could not find the requested post")
                |> return in
    begin match[@warning "-8"] action with
      | "toast" -> Worker.send_task (LocalLike {user;post})
      | "cheer" ->  Worker.send_task (LocalReboost {user;post})
    end;
    let url =
      Dream.query req "redirect"
      |> Option.value ~default:("/feed/") in
    redirect req url
  | _ -> redirect req "/feed"

let route =
  Dream.scope "/post" [] [
    Dream.get "/remote" @@ Error_handling.handle_error_html @@ handle_get_remote;
    Dream.post "/remote" @@ Error_handling.handle_error_html @@ handle_post_remote;
    Dream.get "/:postid" @@ Error_handling.handle_error_html @@ handle_post_get;
    Dream.post "/:postid/toast" @@ Error_handling.handle_error_html @@ handle_post_toast;
    Dream.post "/:postid/cheer" @@ Error_handling.handle_error_html @@ handle_post_cheer;
  ]

