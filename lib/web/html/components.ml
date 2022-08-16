open Containers
open Pure.Utils

let header fields =
  H.header ~a:[H.a_class ["header"]] [
    Pure.menu ~horizontal:true [
      Pure.a_menu_heading ~a:[H.a_href "./"] [H.txt "OCamlot"];
      Pure.menu_list (List.map (fun (name, link) ->
        Pure.menu_item [Pure.menu_link ~a:[(H.a_href link)] [H.txt name]]
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

let navigation_panel ~from_ ~to_ =
  H.div ~a:[H.a_class ["post-navigation"]] @@ List.concat [
    [
      H.a  ~a:[H.a_href "./single-post-second-page.html"; H.a_class ["post-navigation-button"]] [
        H.div ~a:[H.a_class ["post-navigation-button-container"]] [
          H.span [H.txt "<-"];
        ]
      ]
    ];
    List.init (to_ - from_) (fun i ->
      let i = from_ + i in
      H.a  ~a:[H.a_href "./single-post-second-page.html"; H.a_class ["post-navigation-button"]] [
        H.div ~a:[H.a_class ["post-navigation-button-container"]] [
          H.span [H.txt (string_of_int i)];
        ]
      ]
    );
    [
      H.a  ~a:[H.a_href "./single-post-second-page.html"; H.a_class ["post-navigation-button"]] [
        H.div ~a:[H.a_class ["post-navigation-button-container"]] [
          H.span [H.txt "->"];
        ]
      ]
    ];
  ]
