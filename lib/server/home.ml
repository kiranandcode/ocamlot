open Containers
open Common

let log = Logging.add_logger "web.home"


let parse_feed = function
  | "feed" -> Some `Feed
  | "direct" -> Some `Direct
  | "local" -> Some `Local
  | "twkn" -> Some `WholeKnownNetwork
  | p -> log.warning (fun f -> f "unable to parse feed type %s" p); None

let parse_calendar s =
  let (let+) x f = Option.bind x f in
  let+ s = Float.of_string_opt s in
  Some (CalendarLib.Calendar.from_unixfloat s)

let extract_post req (post: Database.Post.t) :
     (< author : < image : Tyxml_xml.uri; name : string; .. >;
       contents : [< Html_types.div_content_fun > `H3 ] Tyxml_html.elt list;
       date : string; stats : < cheers : int; toasts : int; .. >;
       title : string; .. >, _) Lwt_result.t =
  let+ author = Database.Post.author post |> fun p -> Dream.sql req (Database.Link.resolve p) in

  return_ok object
    method author = object
      method name = match author with
          Local l -> Database.LocalUser.username l
        | Remote l -> Database.RemoteUser.username l
      method image = "/static/unknown.png"
    end
    method contents = [ Tyxml.Html.txt (Database.Post.post_source post) ]
    method date = Database.Post.published post |> CalendarLib.Printer.Calendar.to_string
    method stats = object method cheers = 0 method toasts = 0 end
    method title = ""
  end

let route config =
  Dream.get "/home" @@ Error_handling.handle_error_html config @@ (fun req ->
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

    let offset_end = offset_start + 10 in

    let offset = (offset_date, offset_start, offset_end) in

    let+ _current_user = current_user req in
    let+ current_user_link = current_user_link req in

    let feed_elements =
      match feed_ty, current_user_link with
      | `Direct, Some user -> Database.Post.collect_post_direct ~offset user
      | `Feed, Some user -> Database.Post.collect_post_feed ~offset user
      | `WholeKnownNetwork, _ -> Database.Post.collect_post_whole_known_network ~offset
      | _, _ -> Database.Post.collect_post_local_network ~offset in

    let+ feed_results = Dream.sql req feed_elements
                     |> map_err (fun err -> `DatabaseError err) in

    let title = match feed_ty with
      | `Local -> "Local Network"
      | `Direct -> "Direct Messages"
      | `Feed -> "My Feed"
      | `WholeKnownNetwork -> "Whole known Network" in

    let+ posts =
      Lwt_list.map_p (extract_post req) feed_results
      |> Lwt.map Result.flatten_l
      |> map_err (fun e -> `DatabaseError e) in

    tyxml @@ Html.build_page ~title [
      Html.Feed.feed_title title;

      Pure.grid_row (
        List.map (fun post ->
          Pure.grid_col [Html.Feed.feed_item (`Post post)]
        ) posts)
      
    ]
  )
