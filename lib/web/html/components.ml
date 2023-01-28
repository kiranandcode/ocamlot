open Containers
open Pure.Utils


let page_title title =
  Pure.grid_col [
    H.div ~a:[H.a_class ["page-title"]] [
      H.h1 [H.txt title]
    ];
  ]

let header fields =
  H.header ~a:[H.a_class ["header"]] [
    Pure.menu ~horizontal:true [
      Pure.a_menu_heading ~a:[H.a_href "./"] [H.txt "OCamlot"];
      Pure.menu_list (List.map (fun (name, link, is_post) ->
        if is_post
        then Pure.menu_item [
          H.form ~a:[H.a_action link; H.a_method `Post] [
            H.input ~a:[H.a_input_type `Submit; H.a_class ["pure-menu-link"]; H.a_value name] ()
          ]
        ]
        else Pure.menu_item [Pure.menu_link ~a:[(H.a_href link)] [H.txt name]]
      ) fields)
    ]
  ]

let footer elts =
  H.div ~a:[H.a_class ["footer"; "l-box"]] (List.map (fun elt ->
    H.a ~a:[H.a_href elt#link] [H.txt elt#name]
  ) elts |> List.intersperse (H.txt "|"))

let stats_box ?a ?a_class stats =
  H.div ?a:(H.a_class ("stats-box" :: !!a_class) +:: a) [
    H.span [Format.ksprintf ~f:H.txt "%d Cheers" (stats#cheers)];
    H.span [Format.ksprintf ~f:H.txt "%d Toasts" (stats#toasts)];
  ]

let profile_box ?a ?a_class author =
  H.div ?a:(H.a_class ("profile-box" :: !!a_class) +:: a) [
    H.img ~src:(author#image) ~alt:(Format.sprintf "%s's profile picture" (author#name)) ();
    H.b [H.txt (author#name)];
  ]

let numeric_navigation_panel ~from_ ~to_ ~current link =
  H.div ~a:[H.a_class ["post-navigation"]] @@ List.concat [
    if current > from_
    then [
      H.a  ~a:[H.a_href (link (current - 1)); H.a_class ["post-navigation-button"]] [
        H.div ~a:[H.a_class ["post-navigation-button-container"]] [
          H.span [H.txt "<-"];
        ]
      ]
    ]
    else [];
    List.init (to_ - from_ + 1) (fun i ->
      let i = from_ + i in
        H.a  ~a:[H.a_href (link i); H.a_class ["post-navigation-button"]] [
          H.div ~a:[H.a_class ["post-navigation-button-container"]] [
          H.span [
            if i = current
            then H.b [H.txt (string_of_int i)]
            else H.txt (string_of_int i)
          ];
          ]
        ]
    );
    if current + 1 <= to_
    then [
      H.a  ~a:[H.a_href (link (current + 1)); H.a_class ["post-navigation-button"]] [
        H.div ~a:[H.a_class ["post-navigation-button-container"]] [
          H.span [H.txt "->"];
        ]
      ]
    ]
    else [];
  ]

let subnavigation_menu entries =
  Pure.grid_row [
    Pure.grid_col ~a_class:["subnavigation-menu"; "pure-menu"; "pure-menu-horizontal"; "pure-menu-scrollable"]
      (List.map (fun (name, link) ->
         Pure.a_menu_heading ~a:[Tyxml.Html.a_href link] [
           Tyxml.Html.txt name
         ]
       ) entries)
  ]

let search_box ?(fields=[]) ?a ?a_class ?(placeholder="Search...") ?value ?(button_text="🔎") ?(name="search") () =
  Pure.grid_row [
    Pure.grid_col ~a_class:["search-box"] [
      Pure.form ?a ?a_class ([
        H.input ~a:((!! (Option.map (fun v -> [H.a_value v]) value)) @
                    [H.a_input_type `Text; H.a_placeholder placeholder; H.a_name name]) ();
        H.input ~a:[H.a_input_type `Submit; H.a_value button_text] ();
      ]  @
        List.map (fun (k,v) ->
          H.input ~a:[H.a_input_type `Hidden; H.a_name k; H.a_value v] ()
        ) fields
      )
    ]
  ]

  

let post_panel ?a ?a_class post =
  H.div ?a:(H.a_class ("post-panel" :: !!a_class) +:: a) [
    H.div ~a:[H.a_class ["post-date"]] [H.b [H.txt post#date]];
    H.div ~a:[H.a_class ["post-panel-body"]] [
      profile_box ~a_class:["post-panel-author"] post#author;
      H.div ~a:[H.a_class ["post-panel-contents"]] post#contents;
    ];
    H.div ~a:[H.a_class ["post-panel-panel"]] (
      List.map (fun (name, link) ->
          let form_attrs, input_attrs =
            match link with
            | None -> [], [H.a_disabled ()]
            | Some link ->
              [H.a_action link; H.a_method `Post], []  in
          H.div ~a:[H.a_class ["post-panel-like"]] [
            H.form ~a:form_attrs [
              H.input ~a:([H.a_input_type `Submit; H.a_value name] @ input_attrs) ()
            ]
          ]
        ) post#actions
      @ [
        H.div ~a:[H.a_class ["post-panel-spacer"]] [];
        stats_box ~a_class:["feed-stats"] post#stats;
      ])
  ]
