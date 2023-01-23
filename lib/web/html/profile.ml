module H = Tyxml.Html

let profile_image profile =
  H.div ~a:[H.a_class ["profile-image"]] [
    H.img ~src:profile#image ~alt:(Format.sprintf "%s's profile picture" profile#name) ();
  ]

let profile_stats stats =
  H.div ~a:[H.a_class[ "profile-stats"]] [
    H.a ~a:[H.a_href "./profile-followers.html"] [Format.ksprintf H.txt "%d followers" stats#followers];
    H.a ~a:[H.a_href "./profile-following.html"] [Format.ksprintf H.txt "%d following" stats#following];
    H.a ~a:[H.a_href "./profile.html"] [Format.ksprintf H.txt "%d posts" stats#posts];
  ]

let profile ?edit profile =
  H.div ~a:[H.a_class ["profile"]] ([
    profile_image profile;
    H.div ~a:[H.a_class ["profile-details"]] [
      H.div ~a:[H.a_class ["profile-name"]] [H.b [H.txt profile#name]];
      H.div ~a:[H.a_class ["profile-summary"]] profile#details;
      profile_stats profile#stats;
    ];
  ] @ match edit with
  | Some edit_ref -> [
      H.div ~a:[H.a_class ["profile-panel"]] [
        H.div ~a:[H.a_class ["profile-panel-button"]] [
          H.a ~a:[H.a_href edit_ref] [H.txt "🖉"]
        ]
      ]
    ]
  | _ -> [])

let edit_profile ?about ?display_name ?image ~username () =
  let display_name = match display_name with None -> [] | Some display_name -> [H.a_value display_name] in
  let about_value,about_text =
    match about with None -> [H.a_placeholder "About me..."],H.txt "" | Some about -> [], H.txt about in
  H.div ~a:[H.a_class ["profile"]] [
    H.div ~a:[H.a_class ["profile-details"]] [
      H.div ~a:[H.a_class ["profile-name"]] [H.b [H.txt ("Editing " ^ username ^ "'s profile")]];
      H.div ~a:[H.a_class ["profile-summary"]] [
        H.form ~a:[H.a_class ["pure-form"; "pure-form-aligned"]] [
          H.fieldset [
            H.div ~a:[H.a_class ["pure-control-group"]] [
              H.label ~a:[H.a_label_for "user-name"] [H.txt "Username"];
              H.input ~a:[H.a_readonly (); H.a_input_type `Text; H.a_id "user-name"; H.a_value username] ();
            ];
            H.div ~a:[H.a_class ["pure-control-group"]] [
              H.label ~a:[H.a_label_for "display-name"] [H.txt "Display Name"];
              H.input ~a:([H.a_input_type `Text; H.a_id "display-name"; H.a_placeholder "Display Name"] @ display_name) ();
            ];
          ];
          H.div ~a:[H.a_class ["pure-control-group"]] [
              H.label ~a:[H.a_label_for "about"] [H.txt "About"];
              H.textarea ~a:([H.a_class ["pure-input-1-2"]; H.a_id "about"] @ about_value) about_text;
            ];

          H.input ~a:[H.a_input_type `Submit; H.a_value "Update"] ();
        ];
        H.form ~a:[H.a_class ["pure-form"; "pure-form-aligned"]] [
          H.div ~a:[H.a_class ["avatar-update-form"]] [
            H.div ~a:[H.a_class ["avatar-update-image"]] [
              H.img ~src:(Option.value ~default:"/static/images/unknown.png" image) ~alt:(username ^ "'s profile image") ();
            ];
            H.div ~a:[H.a_class ["pure-control-group"]] [
              H.label ~a:[H.a_label_for "avatar"] [H.txt "Avatar"];
              H.input ~a:([H.a_input_type `File; H.a_accept ["png"; "jpg"; "jpeg"; "bmp"]; H.a_id "avatar"]) ();
            ];
          ];
          H.input ~a:[H.a_input_type `Submit; H.a_value "Save"] ();
        ]
      ];
    ];
  ]


let follower follower =
  H.div ~a:[H.a_class ["follower"]] [
    H.div ~a:[H.a_class ["follower-image"]] [
      H.img ~src:follower#image ~alt:(Format.sprintf "%s's profile picture" follower#name) ()
    ];
    H.div ~a:[H.a_class ["follower-name"]] [
      H.span [ H.txt follower#name ];
    ];
    H.div ~a:[H.a_class ["follower-since"]] [
      H.span [H.txt follower#since ]
    ]
  ]
