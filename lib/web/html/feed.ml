module H = Tyxml.Html


let feed_context = function
  | Some (`ToastedBy author) -> 
    [H.div ~a:[H.a_class ["feed-context"]] [
       H.txt "toasted by ";
       H.a ~a:[H.a_href author#link] [H.txt author#name];
     ]]
  | Some (`Reply parent) -> 
    [H.div ~a:[H.a_class ["feed-context"]] [
       H.txt "in reply to ";
       H.a ~a:[H.a_href parent#link] [H.txt parent#title];
       H.txt " by ";
       H.a ~a:[H.a_href parent#author#link] [H.txt parent#author#name];
     ]]
  | None -> []

let post_teaser post =
  H.div ~a:[H.a_class ["feed-post-hidden"; "feed-item"]] [
    Components.profile_box ~a_class:["feed-item-author"] post#author;
    H.div ~a:[H.a_class ["feed-post-content"]] [
      H.div ~a:[H.a_class ["feed-post-date"]] [ H.b [H.txt post#date]; ];
      H.div ~a:[H.a_class ["post-reply-text"]] (
        H.h3 [H.txt post#title] :: post#contents
      );
      Components.stats_box ~a_class:["feed-stats"] post#stats;
    ];
    H.div ~a:[H.a_class ["post-reply-hidden-overlay"]] [
      Pure.button [H.txt "Read Full Post"];
    ]
  ]

let feed_mini_post reply =
  H.div ~a:[H.a_class ["feed-item"]] @@ [
    H.div ~a:[H.a_class ["feed-item-date"]] [H.b [H.txt reply#date]];
    H.div ~a:[H.a_class ["feed-item-body"]] [
      Components.profile_box ~a_class:["feed-item-author"] reply#author;
      H.div ~a:[H.a_class ["feed-item-contents"]] reply#contents;
    ];
    H.div ~a:[H.a_class ["feed-item-panel"]] (
      List.map (fun (name, link) ->
          let form_attrs, input_attrs =
            match link with
            | None -> [], [H.a_disabled ()]
            | Some link ->
              [H.a_action link; H.a_method `Post], []  in
          H.div ~a:[H.a_class ["feed-item-like"]] [
              H.form ~a:form_attrs [
              H.input ~a:([H.a_input_type `Submit; H.a_value name] @ input_attrs) ()
            ]
          ]
        ) reply#actions
      @ [
        H.div ~a:[H.a_class ["feed-panel-spacer"]] [];
        Components.stats_box ~a_class:["feed-stats"] reply#stats;
      ])
  ]

let follow_request req =
  H.div ~a:[H.a_class ["feed-item"; "feed-follow-request"]] [
    Components.profile_box ~a_class:["feed-item-author"] req#from;
    H.div ~a:[H.a_class ["feed-item-contents"]] [
      H.div ~a:[H.a_class ["feed-item-date"]] [
        H.b [H.txt req#date]
      ];
      H.div ~a:[H.a_class ["feed-item-text"]] [
        Format.ksprintf H.txt "Follow request from %s" req#from#name;
      ];
      H.div ~a:[H.a_class ["feed-item-options"]] [
        Pure.button [H.txt "Accept"];
        Pure.button [H.txt "Reject"];
      ]
    ]
  ]

let feed_reply reply =
  H.div ~a:[H.a_class ["feed-item"]] @@ List.flatten [
    feed_context reply#context;
    [
      H.div ~a:[H.a_class ["feed-item-date"]] [H.b [H.txt reply#date]];
      H.div ~a:[H.a_class ["feed-item-body"]] [
        Components.profile_box ~a_class:["feed-item-author"] reply#author;
        H.div ~a:[H.a_class ["feed-item-contents"]] reply#contents;
      ];
      Components.stats_box ~a_class:["feed-stats"] reply#stats;
    ]
  ]

let feed_item = function
  | `FollowRequest req -> follow_request req
  | `Reply reply -> feed_reply reply
  | `Post post -> post_teaser post
  | `MicroPost post -> feed_mini_post post
  | _ -> H.div []
