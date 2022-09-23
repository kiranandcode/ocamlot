module H = Tyxml.Html

let post_reply reply =
  let cls = "post-reply" :: if Option.is_some reply#title then ["post-reply-hidden"] else [] in
  H.div ~a:[H.a_class cls] [
    Components.profile_box ~a_class:["post-reply-author"] reply#author;
    H.div ~a:[H.a_class ["post-reply-content"]] @@ List.concat [
      begin match reply#to_ with
      | None -> []
      | Some to_ ->
        [H.div ~a:[H.a_class ["post-reply-to"]] [
           H.txt "in reply to "; H.a ~a:[H.a_href to_#link] [H.txt to_#name]
         ]]
      end;
      [
        H.div ~a:[H.a_class ["post-reply-date"]] [ H.txt reply#date; ];
        H.div ~a:[H.a_class ["post-reply-text"]] (
          match reply#title with
          | None -> reply#content
          | Some title -> H.h3 [H.txt title] :: reply#content
        );
        Components.stats_box ~a_class:["post-stats"; "post-reply-socials"] reply#stats
      ];
      begin
        if Option.is_some reply#title
        then [H.div ~a:[H.a_class ["post-reply-hidden-overlay"]] [
          Pure.button [H.txt "Read Full Reply"];
        ]]
        else []
      end;
    ]
  ]

let post post =
  H.div ~a:[H.a_class ["post"]] ([
    H.div ~a:[H.a_class ["post-header"]] [
      H.div ~a:[H.a_class ["post-title"]] [
        H.h1 [H.txt post#title];
      ];
      H.div ~a:[H.a_class ["post-sub-title"]] [
        H.i [H.txt post#date];
      ];
      H.div ~a:[H.a_class ["post-links"]] [
        Components.profile_box ~a_class:["post-author"] post#author;
        Components.stats_box ~a_class:["post-stats"] post#stats;
      ];
    ];
    H.div ~a:[H.a_class ["post-content"]] post#content;
  ] @ match post#replies with
  | None -> []
  | Some (replies,no_more) ->
    [H.div ~a:[H.a_class ["post-replies-list"]] @@
     List.map (fun reply -> post_reply reply) replies] @ if no_more > 0 then [
      H.div ~a:[H.a_class ["post-reply"; "post-reply-button"]] [
        Pure.button [Format.ksprintf H.txt "+ %d more..." no_more];
      ]
    ] else []
  )
