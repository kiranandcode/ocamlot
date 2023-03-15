open Containers
open Common

let log = Logging.add_logger "web.feed"
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

(* * Extracting data *)

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

  log.debug (fun f -> f "get of feed");
  let* current_user_link = current_user_link req in
  log.debug (fun f -> f "worked out current user");
  let* feed_elements, feed_element_count =
    begin match feed_ty, current_user_link with
    | `Direct, Some user ->
      let* posts =
        sql req (Database.Posts.collect_direct ~offset ~limit ~start_time ~id:user) in
      let* total_posts =
        sql req (Database.Posts.count_direct ~id:user) in
      return_ok (posts, total_posts)
    | `Feed, Some user ->
      let* posts =
        sql req (Database.Posts.collect_feed ~offset ~limit ~start_time ~id:user) in
      let* total_posts =
        sql req (Database.Posts.count_feed ~id:user) in
      return_ok (posts, total_posts)
    | `WholeKnownNetwork, _ ->
      let* posts = sql req (Database.Posts.collect_twkn ~offset ~limit ~start_time) in
      let* total_posts = sql req (Database.Posts.count_twkn) in
      return_ok (posts, total_posts)        
    | _, _ ->
      let* posts = sql req (Database.Posts.collect_local ~offset ~limit ~start_time) in
      let* total_posts = sql req (Database.Posts.count_local) in
      return_ok (posts, total_posts)
    end
  in
  log.debug (fun f -> f "worked out feed");
  let title = feed_type_to_string feed_ty in

  let* posts =
    Lwt_list.map_p (Extract.extract_post req) feed_elements
    |> Lwt.map Result.flatten_l in
  log.debug (fun f -> f "extracted feed posts");

  let* headers,action = Navigation.build_navigation_bar req in
  log.debug (fun f -> f "built navigation bar");
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
  log.debug (fun f -> f "rendering feed!");
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
                        text=feed_type_to_string ty; form=None}
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
    Dream.get "" @@ Error_handling.handle_error_html @@ handle_feed_get;
  ]
