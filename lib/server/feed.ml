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
  Ptime.of_float_s s

let sql req f = Dream.sql req f |> Lwt_result.map_error (fun err -> `DatabaseError (Caqti_error.show err))

let extract_post req (post: Database.Posts.t) =
  let+ author = post.Database.Posts.author_id
                |> fun p -> sql req (Database.Actor.resolve ~id:p) in
  let+ author_instance =
    match author with
    | `Local _ -> return_ok None
    | `Remote r ->
      let+ r = sql req (Database.RemoteUser.resolve ~id:r) in
      let+ instance =
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

  let+ name = match author with
      `Local l ->
      let+ l = sql req (Database.LocalUser.resolve ~id:l) in
      return_ok l.Database.LocalUser.username
    | `Remote l ->
      let+ l = sql req (Database.RemoteUser.resolve ~id:l) in
      return_ok l.Database.RemoteUser.username in
  let author_obj = object
    method name = name
    method image = "/static/images/unknown.png"
    method instance_url = author_instance
  end in

  return_ok @@ match post.Database.Posts.summary with
  | None ->
    `MicroPost object
      method author = author_obj
      method contents = post_contents
      method date = post.Database.Posts.published |> Ptime.to_float_s |> CalendarLib.Calendar.from_unixfloat
                    |> CalendarLib.Printer.Calendar.to_string
      method stats = object method cheers = 0 method toasts = 0 end
    end
  | Some summary ->
    `Post object
      method author = author_obj
      method contents = post_contents
      method date = post.Database.Posts.published  |> Ptime.to_float_s |> CalendarLib.Calendar.from_unixfloat
                    |> CalendarLib.Printer.Calendar.to_string
      method stats = object method cheers = 0 method toasts = 0 end
      method title = summary
    end

let build_feed_navigation_panel options =
  Html.Components.subnavigation_menu @@
  List.map (fun feed ->
    (feed_type_to_string feed, ("/feed?feed-ty=" ^ encode_feed feed))
  ) options

let route config =
  Dream.get "/feed" @@
  Error_handling.handle_error_html config @@ (fun req ->
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

    let+ current_user_link = current_user_link req in

    let+ feed_elements, feed_element_count =
      sql req @@ fun db ->
      begin match feed_ty, current_user_link with
      | `Direct, Some user ->
        let+ posts =
          Database.Posts.collect_direct
            ~offset ~limit ~start_time ~id:user db in
        let+ total_posts =
          Database.Posts.count_direct ~id:user db in
        return_ok (posts, total_posts)
      | `Feed, Some user ->
        let+ posts =
          Database.Posts.collect_feed
            ~offset ~limit ~start_time ~id:user db in
        let+ total_posts =
          Database.Posts.count_feed ~id:user db in
        return_ok (posts, total_posts)
      | `WholeKnownNetwork, _ ->
        let+ posts = Database.Posts.collect_twkn ~offset ~limit ~start_time db in
        let+ total_posts = Database.Posts.count_twkn db in
        return_ok (posts, total_posts)        
      | _, _ ->
        let+ posts = Database.Posts.collect_local ~offset ~limit ~start_time db in
        let+ total_posts = Database.Posts.count_local db in
        return_ok (posts, total_posts)
      end
    in

    let title = feed_type_to_string feed_ty in

    let+ posts =
      Lwt_list.map_p (extract_post req) feed_elements
      |> Lwt.map Result.flatten_l in

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
            Html.Components.numeric_navigation_panel ~from_:1 ~to_:(feed_element_count / limit + 1) ~current:(offset / limit)
              (fun ind ->
                 Format.sprintf "/feed?feed-ty=%s&offset-time=%f&offset-start=%d"
                   (encode_feed feed_ty) (Ptime.to_float_s start_time)
                   ((ind - 1) * limit)
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
