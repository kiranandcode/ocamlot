open Containers
open Common

let log = Logging.add_logger "web.home"
let limit = 10

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
  let (let+) x f = Option.bind x f in
  let+ s = Float.of_string_opt s in
  Some (CalendarLib.Calendar.from_unixfloat s)

let extract_post req (post: Database.Post.t) =
  let+ author = Database.Post.author post
                |> fun p -> Dream.sql req (Database.Link.resolve p) in
  let+ author_instance =
    match author with
    | Local _ -> return_ok None
    | Remote r ->
      let+ instance =
        Dream.sql req
          (Database.Link.resolve
             (Database.RemoteUser.instance r)) in
      return_ok (Some (Database.RemoteInstance.url instance)) in

  let post_contents =
    let source = Database.Post.post_source post in
    match Database.Post.content_type post with
    | `Markdown ->
      Markdown.markdown_to_html (Omd.of_string source)
    | _ -> [ Tyxml.Html.txt source ] in

  let author_obj = object
    method name = match author with
        Local l -> Database.LocalUser.username l
      | Remote l -> Database.RemoteUser.username l
    method image = "/static/images/unknown.png"
    method instance_url = author_instance
  end in

  return_ok @@ match Database.Post.summary post with
  | None ->
    `MicroPost object
      method author = author_obj
      method contents = post_contents
      method date = Database.Post.published post |> CalendarLib.Printer.Calendar.to_string
      method stats = object method cheers = 0 method toasts = 0 end
    end
  | Some summary ->
    `Post object
      method author = author_obj
      method contents = post_contents
      method date = Database.Post.published post |> CalendarLib.Printer.Calendar.to_string
      method stats = object method cheers = 0 method toasts = 0 end
      method title = summary
    end

let build_feed_navigation_panel options =
  Html.Components.subnavigation_menu @@
  List.map (fun feed ->
    (feed_type_to_string feed, ("/feed?feed-ty=" ^ encode_feed feed))
  ) options

let route config =
  Dream.get "/feed" @@ Error_handling.handle_error_html config @@ (fun req ->
    let feed_ty = Dream.query req "feed-ty"
                  |> Option.flat_map parse_feed
                  |> Option.value ~default:`Local in
    let offset_date =
      Dream.query req "offset-time"
      |> Option.flat_map parse_calendar
      |> Option.value ~default:(CalendarLib.Calendar.now ()) in

    let offset_start =
      Dream.query req "offset-start"
      |> Option.flat_map Int.of_string
      |> Option.value ~default:0 in



    let offset = (offset_date, limit, offset_start * limit) in

    let+ _current_user = current_user req in
    let+ current_user_link = current_user_link req in

    let+ feed_elements, feed_element_count =
      Dream.sql req @@ fun db ->
      begin match feed_ty, current_user_link with
      | `Direct, Some user ->
        let+ posts = Database.Post.collect_post_direct ~offset user db in
        let+ total_posts = Database.Post.collect_post_direct_count user db in
        return_ok (posts, total_posts)
      | `Feed, Some user ->
        let+ posts = Database.Post.collect_post_feed ~offset user db in
        let+ total_posts = Database.Post.collect_post_feed_count user db in
        return_ok (posts, total_posts)
      | `WholeKnownNetwork, _ ->
        let+ posts = Database.Post.collect_post_whole_known_network ~offset db in
        let+ total_posts = Database.Post.collect_post_whole_known_network_count db in
        return_ok (posts, total_posts)        
      | _, _ ->
        let+ posts = Database.Post.collect_post_local_network ~offset db in
        let+ total_posts = Database.Post.collect_post_local_network_count db in
        return_ok (posts, total_posts)
      end
      |> map_err (fun err -> `DatabaseError err)
    in

    let title = feed_type_to_string feed_ty in

    let+ posts =
      Lwt_list.map_p (extract_post req) feed_elements
      |> Lwt.map Result.flatten_l
      |> map_err (fun e -> `DatabaseError e) in

    let+ headers = Navigation.build_navigation_bar req in

    let feed_navigation = match current_user_link with
      | None -> [`Local; `WholeKnownNetwork]
      | Some _ -> [`Feed; `Direct; `Local; `WholeKnownNetwork] in

    let write_post_button = match current_user_link with
      | None -> [ ]
      | Some _ -> [
          Pure.grid_row [
            Pure.grid_col [
              Pure.a_button
                ~a:[Tyxml.Html.a_href "/write"]
                ~a_class:["feed-write-post-button"]
                [Tyxml.Html.txt "Write a new post"]
            ]
          ]
        ] in

    let navigation_panel =
            Html.Components.numeric_navigation_panel ~from_:1 ~to_:(feed_element_count / 10 + 1) ~current:(offset_start + 1)
              (fun ind ->
                 Format.sprintf "/feed?feed-ty=%s&offset-time=%f&offset-start=%d"
                   (encode_feed feed_ty) (CalendarLib.Calendar.to_unixfloat offset_date)
                   (ind - 1)
              ) in
              

    tyxml @@ Html.build_page ~headers ~title @@ List.concat [
      [
        Html.Components.page_title title;
        (build_feed_navigation_panel feed_navigation)
      ];
      write_post_button;

      [
        Pure.grid_row (
          List.map (fun post ->
            Pure.grid_col [Html.Feed.feed_item post]
          ) posts)
      ];

      [
        navigation_panel
      ]

    ]
  )
