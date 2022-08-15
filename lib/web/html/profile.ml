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

let profile profile =
  H.div ~a:[H.a_class ["profile"]] [
    profile_image profile;
    H.div ~a:[H.a_class ["profile-details"]] [
      H.div ~a:[H.a_class ["profile-name"]] [H.b [H.txt profile#name]];
      H.div ~a:[H.a_class ["profile-summary"]] profile#details;
      profile_stats profile#stats;
    ]
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
