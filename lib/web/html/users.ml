module H = Tyxml.Html

let user ?(can_follow=false) user =
  H.div ~a:[H.a_class ["user"]] [
    Components.profile_box user#profile;
    H.div ~a:[H.a_class ["user-item-contents"]] [
      H.div ~a:[H.a_class ["user-item-name"]] [
        H.div ~a:[H.a_class ["user-item-display-name"]] [
          H.b [H.txt user#display_name];
        ];
        H.div ~a:[H.a_class ["user-item-user-name"]] [
          H.a ~a:[H.a_href user#profile_page] [H.txt user#username];
        ]
      ];
      H.div ~a:[H.a_class ["user-item-about"]] user#about;
      H.div ~a:[H.a_class ["user-item-stats"]] [
        H.b [ H.txt (string_of_int user#stats#followers ^ " followers") ];
        H.b [ H.txt (string_of_int user#stats#posts ^ " posts") ];
      ];
      H.div ~a:[H.a_class ["user-item-action"]] (
        match can_follow with
        | false -> []
        | true -> [
            H.form ~a:[
              H.a_action user#follow_link;
              H.a_method `Post
            ] [
              let name, attrs = match user#following with
                | None -> "Request pending", [H.a_disabled ()]
                | Some true -> "Unfollow", []
                | Some false -> "Follow", []   in
              H.input ~a:(attrs @ [
                H.a_input_type `Submit;
                H.a_class ["pure-menu-link"];
                H.a_value name
              ]) ()
            ]
          ])
    ]
  ]
