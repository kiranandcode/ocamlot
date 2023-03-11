open Containers
open Common

let sql req f = Dream.sql req f |> Lwt_result.map_error (fun err -> `DatabaseError (Caqti_error.show err))

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
      sql req (Database.Posts.is_visable_post ~by:current_user ~post:post.Database.Posts.id) in


  if can_access_post
  then 
    let* post_data = Extract.extract_post req post in
    let* headers,action = Navigation.build_navigation_bar req in
    let title =
      Option.value ~default:"(Untitled post)"
        post.Database.Posts.summary in
    tyxml @@ View.Page.render_page title @@ [
      View.Header.render_header ?action headers;
      View.Post.render post_data;
    ]
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
    Worker.send_task Worker.(LocalLike {user;post});
    let url =
      Dream.query req "redirect"
      |> Option.value ~default:("/post/" ^ public_id) in
    redirect req url

let route =
  Dream.scope "/post" [] [
    Dream.get "/:postid" @@ Error_handling.handle_error_html @@ handle_post_get;
    Dream.post "/:postid/toast" @@ Error_handling.handle_error_html @@ handle_post_toast
  ]

