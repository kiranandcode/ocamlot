open Utils

type t = {
  display_name: string;
  username: string;
  profile_picture: string;
  self_link: string;
}

type socials = {
  followers: int;
  following: int;
  actions: link list;
}

let render_user_socials socials =
  div "user-panel" [
    div "user-socials" [
      H.i ~a:[H.a_class ["font-small"]] [H.txt (Format.sprintf "%d followers" socials.followers)];
      H.i ~a:[H.a_class ["font-small"]] [H.txt (Format.sprintf "%d following" socials.following)];
    ];
    H.div ~a:[H.a_class ["user-stats"; "elements-list"]] (
      List.map (fun link ->
        match link.form with
        | None -> H.a ~a:[H.a_href link.url] [H.txt link.text]
        | Some form ->
          H.form ~a:[H.a_action form; H.a_method `Post] [
            H.input ~a:[H.a_value link.text; H.a_input_type `Submit] ();
          ]
      ) socials.actions
      |> intersperse (fun () -> H.p [H.txt "|"]))
  ]

let render_user_box ?(a_class=[]) user elt =
  H.div ~a:[H.a_class ("user-box" :: a_class)] ([
      H.div ~a:[H.a_class ["user-image"; "icon"; "primary-border-small"]] [
        H.img
          ~alt:(Format.sprintf "%s's profile picture" user.display_name)
          ~src:user.profile_picture ();
      ];
      H.div ~a:[H.a_class ["user-details"; "justify-space-around"; "flex-column"]] [
        H.a ~a:[H.a_class ["bold"]] [H.txt user.display_name];
        H.a ~a:[H.a_href user.self_link] [H.txt user.username];
      ];
      spacer ();
      elt
    ])

let render_users_grid users =
  H.div ~a:[H.a_class ["grid"; "users-grid"]] (
    List.map (fun (user, socials) ->
        render_user_box ~a_class:["padding-all"] user
          (render_user_socials socials)
      ) users
  )

let render_users_search_box ?initial_value () =
  div "users-search-box" [
    Form.render_input_form [
      Form.render_input_form_one_line ~a_class:["users-search-box"]
        ~ty:`Text ~value:"search" ~name:"Search" ?initial_value ~submit_name:"Go" ()
    ]
  ]
