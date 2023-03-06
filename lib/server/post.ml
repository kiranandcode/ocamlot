open Containers
open Common

let sql req f = Dream.sql req f |> Lwt_result.map_error (fun err -> `DatabaseError (Caqti_error.show err))

(* * Extracting data *)
let extract_post req (post: Database.Posts.t) =
  let* author = post.Database.Posts.author_id
                |> fun p -> sql req (Database.Actor.resolve ~id:p) in
  let* _author_instance =
    match author with
    | `Local _ -> return_ok None
    | `Remote r ->
      let* r = sql req (Database.RemoteUser.resolve ~id:r) in
      let* instance =
        sql req
          (Database.RemoteInstance.resolve
             ~id:(r.Database.RemoteUser.instance_id)) in
      return_ok (Some (instance.Database.RemoteInstance.url)) in

  let post_contents =
    let source = post.Database.Posts.post_source in
    match post.Database.Posts.content_type with
    | `Markdown ->
      Markdown.markdown_to_html (Omd.of_string source)
    | _ -> [ Tyxml.Html.txt source ] in

  let* post_likes =
    sql req (Database.Likes.count_for_post ~post:post.Database.Posts.id) in

  let* self_url, username, name, image = match author with
      `Local l ->
      let* l = sql req (Database.LocalUser.resolve ~id:l) in
      let username = l.Database.LocalUser.username in
      let self_url = Configuration.Url.user_path username in
      let name =
        Option.value ~default:username
          l.Database.LocalUser.display_name in
      let image =
        Configuration.Url.user_profile_picture
          l.Database.LocalUser.profile_picture in
      return_ok (self_url, username, name, image)
    | `Remote l ->
      let* l = sql req (Database.RemoteUser.resolve ~id:l) in
      let username = l.Database.RemoteUser.username in
      let self_url = l.Database.RemoteUser.url in
      let name =
        Option.value ~default:username
          l.Database.RemoteUser.display_name in
      let image =
        Option.value ~default:"/static/images/unknown.png"
          l.Database.RemoteUser.profile_picture in
      return_ok (self_url, username, name, image) in
  let author_obj =
    View.User.{
      display_name=name;
      username=username;
      profile_picture=image;
      self_link=self_url;
    } in

  return_ok @@ View.Post.{
    headers=[];
    content=post_contents;
    posted_date=post.Database.Posts.published;
    no_toasts= post_likes;
    no_cheers=0;
    has_been_cheered=false; has_been_toasted=false;
    author=author_obj
  } 


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
    let* post_data = extract_post req post in
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









let route =
  Dream.scope "/post" [] [
    Dream.get "/:postid" @@ Error_handling.handle_error_html @@ handle_post_get;
    (* Dream.post "/:postid/like" @@ Error_handling.handle_error_html @@ handle_post_like *)
  ]

