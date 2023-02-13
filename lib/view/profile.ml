open Utils

type t = {
  user: User.t;
  actions: (bool * link) list;
  followers: int;
  following: int;
  details: Html_types.div_content_fun H.elt list;
  details_source: string;
}

let render_profile_header profile =
  H.div ~a:[H.a_class ["profile-header"; "flex"]] [
    div "profile-header-content" [
      H.div ~a:[H.a_class ["profile-image"; "icon"; "icon-large"; "primary-border-small"]] [
        H.img
          ~src:profile.user.profile_picture
          ~alt:(Format.sprintf "%s's profile picture"
                  profile.user.display_name) ()
      ];
      div "profile-header-details" [
        H.a ~a:[H.a_class ["bold"]] [H.txt profile.user.display_name];
        H.a [H.txt profile.user.username]
      ]
    ];
    spacer ();
    div "profile-header-stats-content" [
      H.div ~a:[H.a_class ["profile-header-actions"; "elements-list"]]
        (List.map (fun (enabled, action) ->
             H.a ~a:[H.a_class
                       (if enabled then ["disabled"; "b"] else ["b"]);
                     H.a_href action.url] [
               H.txt action.text
             ]
           ) profile.actions
         |> intersperse (fun () -> H.p [H.txt "|"]));
      div "profile-header-social" [
        H.i ~a:[H.a_class ["font-small"]] [
          H.txt (Format.sprintf "%d followers" profile.followers);
        ];
        H.i ~a:[H.a_class ["font-small"]] [
          H.txt (Format.sprintf "%d following" profile.following);
        ]          
      ]
    ]
  ]

let render_profile (profile: t) =
  div "profile" [
    render_profile_header profile;
    H.div ~a:[H.a_class ["profile-details"; "padding-all"]] [
      H.div ~a:[H.a_class ["profile-details-text"; "text-justify"]]
        profile.details
    ]
  ]


let render_update_profile_box (profile: t) =
  div "update-profile-box" [
    Form.render_input_form [
      Form.render_input_form_entry ~disabled:true ~initial_value:profile.user.username ~ty:`Text
        ~value:"username" ~name:"Username" ();
      Form.render_input_form_entry ~initial_value:profile.user.display_name ~ty:`Text
        ~value:"display-name" ~name:"Display Name" ();
      Form.render_input_form_checkbox ~value:"manually-accepts-followers"
        ~name:"Manually accepts followers" ();
      Form.render_input_form_textarea ~value:"about" ~name:"About"
        ~initial_value:(H.txt profile.details_source) ()
    ]
  ]
