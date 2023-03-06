open Containers
open Common

let log = Logging.add_logger "web.home"
let limit = 10

(* * Utilities  *)
let parse_feed = function
  | "feed" -> Some `Feed
  | "direct" -> Some `Direct
  | "local" -> Some `Local
  | "twkn" -> Some `WholeKnownNetwork
  | p -> log.warning (fun f -> f "unable to parse feed type %s" p); None

let encode_feed = function
  | `Feed  -> "feed"
  | `Direct  -> "direct"
  | `Local  -> "local"
  | `WholeKnownNetwork  -> "twkn"

let feed_type_to_string = function
  | `Local -> "Local Network"
  | `Direct -> "Direct Messages"
  | `Feed -> "My Feed"
  | `WholeKnownNetwork -> "Whole-known-network"

let parse_calendar s =
  let (let*) x f = Option.bind x f in
  let* s = Float.of_string_opt s in
  Ptime.of_float_s s

let sql req f = Dream.sql req f |> Lwt_result.map_error (fun err -> `DatabaseError (Caqti_error.show err))

(* * Extracting data *)
let extract_post req (post: Database.Posts.t) =
  let* author = post.Database.Posts.author_id
                |> fun p -> sql req (Database.Actor.resolve ~id:p) in
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

  let post_contents =
    let source = post.Database.Posts.post_source in
    match post.Database.Posts.content_type with
    | `Markdown ->
      Markdown.markdown_to_html (Omd.of_string source)
    | _ -> [ Tyxml.Html.txt source ] in

  let* post_likes =
    sql req (Database.Likes.count_for_post ~post:post.Database.Posts.id) in

  let* self_link, name, username, image = match author with
      `Local l ->
      let* l = sql req (Database.LocalUser.resolve ~id:l) in
      let username = l.Database.LocalUser.username in
      let self_link = Configuration.Url.user_path username in
      let name =
        Option.value ~default:l.Database.LocalUser.username
          l.Database.LocalUser.display_name in
      let image = Option.map Configuration.Url.image_path
          l.Database.LocalUser.profile_picture in
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
  let author_obj =
    View.User.{
      display_name=name;
      username;
      profile_picture=Configuration.Url.user_profile_picture image;
      self_link;
    } in

  return_ok @@
  View.Post.{
    headers=[];
    content=post_contents;
    posted_date=post.Database.Posts.published;
    no_toasts=post_likes;
    no_cheers=0;
    has_been_toasted=false;
    has_been_cheered=false;
    author=author_obj
  }

(* let build_feed_navigation_panel options = *)
(*   Html.Components.subnavigation_menu @@ *)
(*   List.map (fun feed -> *)
(*     (feed_type_to_string feed, ("/feed?feed-ty=" ^ encode_feed feed)) *)
(*   ) options *)

(* * Feed Html *)
let handle_feed_get req =
  let feed_ty = Dream.query req "feed-ty"
                |> Option.flat_map parse_feed
                |> Option.value ~default:`Local in

  let start_time =
    Dream.query req "offset-time"
    |> Option.flat_map parse_calendar
    |> Option.value ~default:(Ptime_clock.now ()) in

  let offset =
    Dream.query req "offset-start"
    |> Option.flat_map Int.of_string
    |> Option.value ~default:0 in

  let* current_user_link = current_user_link req in

  let* feed_elements, feed_element_count =
    sql req @@ fun db ->
    begin match feed_ty, current_user_link with
    | `Direct, Some user ->
      let* posts =
        Database.Posts.collect_direct
          ~offset ~limit ~start_time ~id:user db in
      let* total_posts =
        Database.Posts.count_direct ~id:user db in
      return_ok (posts, total_posts)
    | `Feed, Some user ->
      let* posts =
        Database.Posts.collect_feed
          ~offset ~limit ~start_time ~id:user db in
      let* total_posts =
        Database.Posts.count_feed ~id:user db in
      return_ok (posts, total_posts)
    | `WholeKnownNetwork, _ ->
      let* posts = Database.Posts.collect_twkn ~offset ~limit ~start_time db in
      let* total_posts = Database.Posts.count_twkn db in
      return_ok (posts, total_posts)        
    | _, _ ->
      let* posts = Database.Posts.collect_local ~offset ~limit ~start_time db in
      let* total_posts = Database.Posts.count_local db in
      return_ok (posts, total_posts)
    end
  in

  let title = feed_type_to_string feed_ty in

  let* posts =
    Lwt_list.map_p (extract_post req) feed_elements
    |> Lwt.map Result.flatten_l in

  let* headers,action = Navigation.build_navigation_bar req in

  let feed_navigation = match current_user_link with
    | None -> [`Local; `WholeKnownNetwork]
    | Some _ -> [`Feed; `Direct; `Local; `WholeKnownNetwork] in

  let navigation_panel =
    View.Components.render_pagination_numeric
      ~start:1 ~stop:(feed_element_count / limit + 1)
      (* ~current:(offset/limit) *)
      (fun ind ->
         Format.sprintf "/feed?feed-ty=%s&offset-time=%f&offset-start=%d"
           (encode_feed feed_ty) (Ptime.to_float_s start_time)
           ((ind - 1) * limit)
      ) () in

  tyxml @@ View.Page.render_page title @@ List.concat [
    [View.Header.render_header ?action headers];
    [
      View.Components.render_heading
        ~icon:(List.find_idx (Equal.poly feed_ty) feed_navigation
               |> Option.map fst
               |> Option.value ~default:(-1)
               |> ((+) 1)
               |> string_of_int)
        ~current:(feed_type_to_string feed_ty)
        ~options:(List.map (fun ty ->
            View.Utils.{url=Format.sprintf "/feed?feed-ty=%s" (encode_feed ty);
                        text=feed_type_to_string ty}
          ) feed_navigation) ();
    ];

    [
      View.Post_grid.render_post_grid posts
    ];

    [
      navigation_panel
    ]

  ]

(* * Route *)
let route =
  Dream.scope "/feed" [] [
    Dream.get "/" @@ Error_handling.handle_error_html @@ handle_feed_get;
  ]
