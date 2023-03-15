open Utils

type t = {
  self_link: Uri.t option;
  toast_link: Uri.t option;
  cheer_link: Uri.t option;
  reply_link: Uri.t option;

  headers: (string * User.t) list;
  content: Html_types.div_content_fun H.elt list;
  posted_date: Ptime.t;
  no_toasts: int; has_been_toasted: bool;
  no_cheers: int; has_been_cheered: bool;
  attachments: (bool * string) list;
  author: User.t;
}

let render_post_author (author: User.t) =
  H.div ~a:[H.a_class ["post-author"]] [
    H.div ~a:[H.a_class [
        "post-author-details"; "justify-space-around"; "flex-column"
      ]] [
      H.a ~a:[H.a_class ["bold"]] [H.txt author.display_name];
      H.a ~a:[H.a_href author.self_link] [H.txt author.username]
    ];
    H.div ~a:[H.a_class [
        "post-author-image"; "icon"; "primary-border-small"
      ]] [
      H.img
        ~src:author.profile_picture
        ~alt:(Format.sprintf "%s's profile picture"
                author.display_name) ();
    ]
  ]

let print_stat stat value has_been_activated =
  let value, ext = if has_been_activated then value - 1, "(+1)" else value, "" in
  Format.sprintf "%d%s %s" value ext stat

let render_post_headers = function
  | [] -> []
  | headers ->
    [H.div ~a:[H.a_class ["post-box-header"]]
      (List.map
         (fun (header, author) ->
            H.b ~a:[H.a_class ["bold"]] [
              H.txt (Format.sprintf "%s by " header);
              H.a ~a:[H.a_href author.User.self_link] [
                H.txt (author.User.username)
              ]
            ]
         ) headers)]

let render ?(render_attachments=false) ?redirect_url ?a_class post =
  H.div ~a:[H.a_class (Option.to_list a_class @ ["post-box"; "padding-all"])]
    (List.concat [
       render_post_headers post.headers;
       [
         H.div ~a:[H.a_class ["post-text"; "text-justify"]] (
           post.content @
           match post.attachments with
           | [] -> []
           | attachments ->
             if render_attachments
             then
               let images, files = List.partition (fun (is_img, _) -> is_img) attachments in
               [

                 H.div ~a:[H.a_class ["post-attachments"]] (
                   [
                     H.div ~a:[H.a_class ["post-attachment-images"]] (List.mapi (fun i (_, img) ->
                       H.img ~src:img ~alt:(Format.sprintf "Attached image %d" i) ()
                     ) images);
                     H.div ~a:[H.a_class ["post-attachment-files"]] (List.mapi (fun i (_, file) ->
                       H.a ~a:[H.a_href file] [H.txt (Format.sprintf "Attached file %d" i)]
                     ) files)
                   ]
                 )
               ]
             else [
               H.div ~a:[H.a_class ["post-attachments"]] (List.mapi (fun i (is_image, img) ->
                 H.a ~a:[H.a_href img] [H.txt (Format.sprintf "Attached %s %d" (if is_image then "image" else "file") i)]
               ) attachments)
             ]
         );
         H.div ~a:[H.a_class ["post-details"; "font-small"; "flex"]] [
           H.div ~a:[H.a_class ["post-stats"; "justify-space-around"; "flex-column"]] [
             H.div ~a:[H.a_class ["post-date"]] [
               let posted_date =
                 H.i [ H.txt (Format.asprintf "Posted on %a" pp_date (Ptime.to_date post.posted_date))] in
               (match post.self_link with
                | None -> posted_date
                | Some link -> H.a ~a:[H.a_href (Uri.to_string link)] [posted_date])
             ];
             H.div ~a:[H.a_class ["post-social"]] (
               let input_form url stat =
                 H.form ~a:[ H.a_action url; H.a_method `Post] [
                   H.input ~a:[ H.a_input_type `Submit; H.a_value stat] ()
                 ] in
               let reply_action =
                 match post.reply_link with
                 | None -> []
                 | Some reply_link ->
                   [H.a ~a:[H.a_href (Uri.to_string reply_link)] [H.txt "Reply"]] in
               match post.toast_link, post.cheer_link with
               | Some toast_link, Some cheer_link -> [
                   input_form
                     (Uri.to_string 
                        (match redirect_url with
                           None -> toast_link
                         | Some url ->
                           Uri.add_query_param toast_link ("redirect", [url])))
                     (print_stat "toasts" post.no_toasts post.has_been_toasted);
                   input_form
                     (Uri.to_string 
                        (match redirect_url with
                           None -> cheer_link
                         | Some url ->
                           Uri.add_query_param toast_link ("redirect", [url])))
                     (print_stat "cheers" post.no_cheers post.has_been_cheered);
                 ] @ reply_action
               | _ -> [
                   H.a [
                     H.txt (print_stat "toasts" post.no_toasts post.has_been_toasted)
                   ];
                   H.a [
                     H.txt (print_stat "cheers" post.no_cheers post.has_been_cheered)
                   ];
                 ]
             )
           ];
           render_post_author post.author
         ]
       ]])
