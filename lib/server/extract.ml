open Containers
open Common

let extract_user_profile req (user: Database.LocalUser.t) : (View.Profile.t, _) Lwt_result.t =
  let* no_following, no_followers, _no_posts =
    sql req (fun db ->
      let* user = Database.Actor.create_local_user ~local_id:(user.Database.LocalUser.id) db in
      let* following = Database.Follows.count_following ~author:user db in
      let* followers = Database.Follows.count_followers ~target:user db in
      let* posts = Database.Posts.count_posts_by_author ~author:user db in
      Lwt.return_ok (following, followers,posts)
    ) in
  let+ current_user = current_user req in
  let actions = match current_user with
    | Some current_user when String.equal current_user.username user.username ->
      View.Utils.[true, {url="/users/" ^ user.username ^ "/edit"; text="Edit"; form=None}]
    | _ ->
      View.Utils.[true, {url="/users/" ^ user.username ^ "/follow"; text="Follow"; form=None};
                  false, {url="/users/" ^ user.username ^ "/mute"; text="Mute"; form=None};
                  false, {url="/users/" ^ user.username ^ "/block"; text="Block"; form=None}] in
  View.Profile.{
    user=View.User.{
      display_name=Option.value ~default:user.username user.display_name;
      username=user.username;
      profile_picture=
        Configuration.Url.user_profile_picture
          user.Database.LocalUser.profile_picture;
      self_link=
        Configuration.Url.user_path user.Database.LocalUser.username;
    };
    actions;
    followers=no_followers;
    following=no_following;
    details=[Tyxml.Html.txt (Option.value user.about ~default:"")];
    details_source=Option.value user.about ~default:""
  } 

let extract_user req author =
  let* author_instance =
    match author with
    | `Local _ -> return_ok None
    | `Remote r ->
      let* r = sql req (Database.RemoteUser.resolve ~id:r) in
      let* instance =
        sql req
          (Database.RemoteInstance.resolve
             ~id:(r.Database.RemoteUser.instance_id)) in
      return_ok (Some (instance.Database.RemoteInstance.url)) in
  let* self_link, name, username, image = match author with
      `Local l ->
      let* l = sql req (Database.LocalUser.resolve ~id:l) in
      let username = l.Database.LocalUser.username in
      let self_link = Configuration.Url.user_path username in
      let name =
        Option.value ~default:l.Database.LocalUser.username
          l.Database.LocalUser.display_name in
      let image = l.Database.LocalUser.profile_picture in
      return_ok (self_link, name, username, image)
    | `Remote l ->
      let* l = sql req (Database.RemoteUser.resolve ~id:l) in
      let username = l.Database.RemoteUser.username ^ "@" ^
                     Option.value ~default:"" author_instance in
      let self_link = l.Database.RemoteUser.url in
      let name =
        Option.value ~default:l.Database.RemoteUser.username
          l.Database.RemoteUser.display_name in
      let image = l.Database.RemoteUser.profile_picture in
      return_ok (self_link, name, username, image) in
  return_ok View.User.{
    display_name=name;
    username;
    profile_picture=Configuration.Url.user_profile_picture image;
    self_link;
  }


let extract_user_socials req author : (View.User.socials, _) Lwt_result.t =
  let* followers = sql req (Database.Follows.count_followers ~target:author) in
  let* following = sql req (Database.Follows.count_following ~author:author) in
  let* current_user = current_user_link req in
  let* actions = match current_user with
    | None -> return_ok []
    | Some current_user when (current_user:>int) = (author:>int) -> return_ok []
    | Some current_user ->
      let* follow = sql req (Database.Follows.find_follow_between
                               ~author:current_user ~target:author) in
      let* user = sql req (Database.Actor.resolve ~id:author) in
      let+ user = match user with
        | `Local id ->
          let+ user = sql req (Database.LocalUser.resolve ~id) in
          user.username
        | `Remote id ->
          let* user = sql req (Database.RemoteUser.resolve ~id) in
          let+ remote_instance = sql req (Database.RemoteInstance.resolve ~id:user.instance_id) in
          user.Database.RemoteUser.username ^ "@" ^  remote_instance.url in
      match follow with
      | None -> [View.Utils.{text="Follow"; url=""; form=Some ("/users/" ^ user ^ "/follow")}]
      | Some _ -> [View.Utils.{text="Unfollow"; url=""; form=Some ("/users/" ^ user ^ "/unfollow")}] in
  return_ok View.User.{
    followers;
    following;
    actions;
  }

let extract_post req (post: Database.Posts.t) =
  let* author = post.Database.Posts.author_id
                |> fun p -> sql req (Database.Actor.resolve ~id:p) in
  let* current_user = current_user_link req in

  let post_contents =
    let source = post.Database.Posts.post_source in
    match post.Database.Posts.content_type with
    | `Markdown ->
      Markdown.markdown_to_html (Omd.of_string source)
    | _ -> [ Tyxml.Html.txt source ] in

  let* author_obj = extract_user req author in

  let* post_likes =
    sql req (Database.Likes.count_for_post ~post:post.Database.Posts.id) in
  let* has_been_toasted =
    match current_user with
    | None -> return_ok false
    | Some author ->
      let+ like = sql req (Database.Likes.find_like_between ~post:post.id ~author) in
      Option.is_some like in

  let* post_cheers =
    sql req (Database.Reboosts.count_for_post ~post:post.Database.Posts.id) in
  let* has_been_cheered =
    match current_user with
    | None -> return_ok false
    | Some author ->
      let+ reboost = sql req (Database.Reboosts.find_reboost_between ~post:post.id ~author) in
      Option.is_some reboost in


  let self_link =
    match post.public_id with
    | None -> Configuration.Url.remote_post_path post.url
    | Some id -> Uri.of_string (Configuration.Url.post_path id) in

  let* headers =
    match current_user with
    | None -> return_ok []
    | Some user ->
      let* reboosts =
        sql req (Database.Reboosts.collect_relevant_for_user ~post:post.id ~user) in
      let* headers =
        Lwt_list.map_s (fun reboost ->
            let* user = sql req (Database.Actor.resolve ~id:reboost.Database.Reboosts.actor_id) in
            let+ user = extract_user req user in
            ("Toasted", user)
          ) reboosts >> Result.flatten_l in
      return_ok headers in

  let toast_link =
    match post.public_id with
    | None -> Configuration.Url.remote_post_toast post.url
    | Some id -> Uri.of_string (Configuration.Url.post_path id ^ "/toast") in

  let cheer_link =
    match post.public_id with
    | None -> Configuration.Url.remote_post_cheer post.url
    | Some id -> Uri.of_string (Configuration.Url.post_path id ^ "/cheer") in

  return_ok @@
  View.Post.{
    self_link=Some self_link; toast_link=Some toast_link; cheer_link=Some cheer_link; reply_link=None;
    headers;
    content=post_contents;
    posted_date=post.Database.Posts.published;

    no_toasts=post_likes; has_been_toasted;
    no_cheers=post_cheers; has_been_cheered;

    author=author_obj
  }

let extract_follow_request req (follow_request: Database.Follows.t) : (View.Follow_requests.t, _) Lwt_result.t =
  let public_id = Option.get_exn_or "expected public id" follow_request.public_id in
  let* author = sql req (Database.Actor.resolve ~id:follow_request.author_id) in
  let* user = extract_user req author in
  let date = follow_request.created in
  return_ok View.Follow_requests.{
    user;
    date;
    actions=[
      {url=Format.sprintf "/follow-requests/%s/accept" public_id; text="Accept";form=None};
      {url=Format.sprintf "/follow-requests/%s/reject" public_id; text="Reject";form=None}
    ]
  }
