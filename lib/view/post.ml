open Utils

type t = {
  headers: (string * User.t) list;
  content: Html_types.div_content_fun H.elt list;
  posted_date: Ptime.t;
  no_toasts: int; has_been_toasted: bool;
  no_cheers: int; has_been_cheered: bool;
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

let render ?a_class post =
  H.div ~a:[H.a_class (Option.to_list a_class @ ["post-box"; "padding-all"])]
    (List.concat [
        render_post_headers post.headers;
        [
          H.div ~a:[H.a_class ["post-text"; "text-justify"]] (post.content);
          H.div ~a:[H.a_class ["post-details"; "font-small"; "flex"]] [
            H.div ~a:[H.a_class ["post-stats"; "justify-space-around"; "flex-column"]] [
              H.div ~a:[H.a_class ["post-date"]] [
                H.i [ H.txt (Format.asprintf "Posted on %a" pp_date
                               (Ptime.to_date post.posted_date))]
              ];
              H.div ~a:[H.a_class ["post-social"]] [
                H.a [ H.txt (print_stat "toasts" post.no_toasts post.has_been_toasted)];
                H.a [ H.txt (print_stat "cheers" post.no_cheers post.has_been_cheered)];
              ]
            ];
            render_post_author post.author
          ]
        ]])