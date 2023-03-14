open Containers
open Common

let log = Logging.add_logger "web.write"

let check_authenticated = Common.Middleware.enforce_present "user" ~else_:"/feed"

let encode_content_type = function
  | `Markdown -> "markdown" 
  | `Text -> "text" 
  | `Org -> "org" 
let parse_content_type = function
  | "markdown" -> Ok (`Markdown)
  | "text" -> Ok (`Text)
  | "org" -> Ok (`Org)
  | ct -> Error (Format.sprintf "unsupported content type %s" ct)

let encode_scope = function
  | `Public -> "public" 
  | `Followers -> "followers" 
  | `DM -> "direct-message" 
let parse_scope = function
  | "public" -> Ok (`Public)
  | "followers" -> Ok (`Followers)
  | "direct-message" -> Ok (`DM)
  | ct -> Error (Format.sprintf "unsupported scope type %s" ct)


let handle_get_write ?(errors=[]) ?title ?to_ ?content_type ?visibility ?contents req =
  let in_reply_to = Dream.query req "in-reply-to" in
  let* current_user = current_user req in
  let* user = match current_user with
    | None -> return_ok None
    | Some user ->
      let+ user = Extract.extract_user_profile req user in
      Some user in
  match user with
  | None -> redirect req "/feed"
  | Some user ->
    let* headers,action = Navigation.build_navigation_bar req in
    let token = Dream.csrf_token req in

    let preview = match content_type, contents with
      | Some `Markdown, Some contents ->
        Some (Markdown.markdown_to_html (Omd.of_string contents))
      | _ -> None in

    let in_reply_to_fields = Option.map (fun v -> "in-reply-to", v) in_reply_to |> Option.to_list in

    let write_action = (match in_reply_to with
          None -> Configuration.Url.write_post_path
        | Some url -> Configuration.Url.write_reply_path url) in

    let* reply_post =
      match in_reply_to with
      | None -> return_ok None
      | Some url ->
        (sql req (Database.Posts.lookup_by_url ~url)) in

    let* reply_post =
      match reply_post with
      | None -> return_ok None
      | Some post ->
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
          let* post = Extract.extract_post req post in
          return_ok (Some post)
        else return_ok None in

    tyxml @@ View.Page.render_page "Write a new post" (List.concat [
        [
          View.Header.render_header ?action headers;
          View.Components.render_heading
            ~icon:"W" ~current:"Write a post" ~actions:[
            { text="Preview"; url="preview"; form=Some "write-post-form" };
            { text="Submit"; url="submit"; form=Some "write-post-form" }
          ] ()
        ];
        (match errors with [] -> [] | errors -> [View.Error.render_error_list errors]);
        [        
          View.Write_post_box.render_write_post_box
            ~fields:(["dream.csrf", token] @ in_reply_to_fields)
            ~action:(Uri.to_string write_action) ~id:"write-post-form"
            ?title ?to_:(Option.map (String.concat ", ") to_)
            ?content_type:(Option.map encode_content_type content_type)
            ?visibility:(Option.map encode_scope visibility)
            ?message:contents
            ()
        ];
        (match preview with
         | Some preview -> [
             View.Components.render_heading
               ~icon:"P" ~current:"Preview" ();
             View.Write_post_box.render_write_post_preview
               View.Post.{
                 self_link=None; toast_link=None; cheer_link=None; reply_link=None;
                 headers=[];
                 content=preview;
                 posted_date=Ptime_clock.now ();
                 no_toasts=10;
                 no_cheers=5;
                 has_been_cheered=false; has_been_toasted=false;
                 author=user.user
               };
           ]
         | _ -> []);
        (match reply_post with
         | Some post ->
           [
             View.Components.render_heading
               ~icon:"R" ~current:"In Reply To" ();
             View.Post.render post
           ]
         | _ -> []
        )
      ])

let handle_post_write req =
  let* data = Dream.form req |> sanitize_form_error ([%show: (string * string) list]) in
  log.info (fun f -> f ~request:req "got post to write with %s"
               ([%show: (string * string) list] data));
  let res =
    let open VResult in
    let title = form_data "title" data |> Result.to_opt |> Option.filter (Fun.negate String.is_empty) in
    let to_ = form_data "to" data
              |> Result.to_opt
              |> Option.map (String.split_on_char ',')
              |> Option.map (List.map String.trim)
              |> Option.map (List.filter (Fun.negate String.is_empty))
              |> Option.filter (Fun.negate List.is_empty)  in
    let in_reply_to = form_data "in-reply-to" data |> Result.to_opt
                      |> Option.filter (Fun.negate String.is_empty) in
    let* contents = lift (form_data "message" data) in
    let* content_type = lift (form_data "content-type" data) in
    let* content_type = lift (parse_content_type content_type) in
    let* visibility = lift (form_data "visibility" data) in
    let* visibility = lift (parse_scope visibility) in
    let* () = ensure "Post contents can not be empty" (not @@ String.is_empty contents) in
    let is_preview = form_data "preview" data |> Result.to_opt |> Option.is_some in
    Ok (title, to_, content_type, visibility, contents, is_preview, in_reply_to) in

  match res with
  | Error errors ->
    let title = form_data "title" data |> Result.to_opt |> Option.filter (Fun.negate String.is_empty) in
    let to_ = form_data "to" data |> Result.to_opt |> Option.map (String.split_on_char ',') in
    let contents = form_data "message" data |> Result.to_opt in
    let content_type = form_data "content-type" data |> Result.flat_map parse_content_type |> Result.to_opt in
    handle_get_write ~errors ?title ?to_ ?content_type ?contents req
  (* if is a preview request, then just handle get write *)
  | Ok (title, to_, content_type, visibility, contents, true, _) ->
    handle_get_write ?title ?to_ ~content_type ~visibility ~contents req
  | Ok (title, to_, content_type, scope, contents, false, in_reply_to) ->
    log.info (fun f -> f "received get request %s"
                         ([%show: string option * string list option *
                                  [> `Markdown | `Org | `Text ] *
                                  [> `DM | `Followers | `Public ] * string]
                            (title, to_, content_type, scope, contents)));
    let* Some user = current_user req in
    let () = Worker.send_task
               ((LocalPost {
                 user=user;  title; scope; content_type;
                 post_to=to_; in_reply_to;
                 content=contents
               })) in
    redirect req "/feed"
[@@warning "-8"]

let route =
  Dream.scope "/write" [check_authenticated] [
    Dream.get "" @@ Error_handling.handle_error_html @@ handle_get_write;
    Dream.post "" @@ Error_handling.handle_error_html @@ handle_post_write;
  ]
