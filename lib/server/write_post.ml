open Containers
open Common

let log = Logging.add_logger "web.write"

let check_authenticated = Common.Middleware.enforce_present "user" ~else_:"/feed"

let parse_content_type = function
  | "markdown" -> Ok (`Markdown)
  | "text" -> Ok (`Text)
  | "org" -> Ok (`Org)
  | ct -> Error (Format.sprintf "unsupported content type %s" ct)

let parse_scope = function
  | "public" -> Ok (`Public)
  | "followers" -> Ok (`Followers)
  | "direct-message" -> Ok (`DM)
  | ct -> Error (Format.sprintf "unsupported scope type %s" ct)


let handle_get_write ?errors ?title ?to_ ?content_type ?scope:_ ?contents req =
  let _context = Dream.query req "context" in
  let* headers = Navigation.build_navigation_bar req in
  let token = Dream.csrf_token req in

  let preview = match content_type, contents with
    | Some `Markdown, Some contents ->
      Some (Markdown.markdown_to_html (Omd.of_string contents))
    | _ -> None in


  tyxml @@ Html.build_page ~headers ~title:"Write a new post" [
    Html.Write_post.write_post_title ();
    Pure.grid_row @@ List.concat [
      match preview with
      | None ->
        [Pure.grid_col [
           Html.Write_post.write_post_box ?errors ?title ?to_ ?contents ~fields:["dream.csrf", token] ();
         ]]
      | Some preview -> [
          Pure.grid_col_responsive [`sm, (1,2)] [
            Html.Write_post.write_post_box ?errors ?title ?to_ ?contents ~fields:["dream.csrf", token] ();
          ];
          Pure.grid_col_responsive ~a_class:["write-post-preview"] [`sm, (1,2)] [
            Tyxml.Html.div               preview
          ];
        ]
    ]
  ]

let handle_post_write req =
  let* data = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  log.info (fun f -> f ~request:req "got post to write with %s" ([%show: (string * string) list] data));

  let res =
    let open VResult in
    let title = form_data "title" data |> Result.to_opt |> Option.filter (Fun.negate String.is_empty) in
    let post_to = form_data "to" data |> Result.to_opt in

    let* contents = lift (form_data "contents" data) in
    let* content_type = lift (form_data "content-type" data) in
    let* content_type = lift (parse_content_type content_type) in
    let* scope = lift (form_data "scope" data) in
    let* scope = lift (parse_scope scope) in
    let* () = ensure "Post contents can not be empty" (not @@ String.is_empty contents) in
    let is_preview = form_data "preview-button" data |> Result.to_opt |> Option.is_some in
    Ok (title, post_to, content_type, scope, contents, is_preview) in

  match res with
  | Error errors ->
    let title = form_data "title" data |> Result.to_opt |> Option.filter (Fun.negate String.is_empty) in
    let to_ = form_data "to" data |> Result.to_opt in
    let contents = form_data "contents" data |> Result.to_opt in
    let content_type = form_data "content-type" data |> Result.flat_map parse_content_type |> Result.to_opt in
    handle_get_write ~errors ?title ?to_ ?content_type ?contents req
  (* if is a preview request, then just handle get write *)
  | Ok (title, to_, content_type, scope, contents, true) ->
    handle_get_write ?title ?to_ ~content_type ~scope ~contents req
  | Ok (title, post_to, content_type, scope, contents, false) ->
    log.info (fun f -> f "received get request %s"
                         ([%show: string option * string option *
                                  [> `Markdown | `Org | `Text ] *
                                  [> `DM | `Followers | `Public ] * string]
                            (title, post_to, content_type, scope, contents)));
    let* Some user = current_user req in
    let () = Worker.send_task
               Worker.((LocalPost {
                 user=user;  title; scope; content_type;
                 post_to=post_to |> Option.map (String.split_on_char ',' ); 
                 content=contents
               })) in

    redirect req "/feed"
[@@warning "-8"]

let route =
  Dream.scope "/write" [check_authenticated] [
    Dream.get "" @@ Error_handling.handle_error_html @@ handle_get_write;
    Dream.post "" @@ Error_handling.handle_error_html @@ handle_post_write;

  ]
