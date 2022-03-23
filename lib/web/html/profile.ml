open Common

let build  _req (following, followers, posts) user =
  H.div ~a:[H.a_class ["hero"]] [
    H.div ~a:[H.a_class ["container"]] [
      H.div ~a:[H.a_class ["card"]] [
        H.div ~a:[H.a_class ["card-image"]] [
          H.figure ~a:[H.a_class ["image"; "is-4by3"]] [
            H.img ~src:"https://bulma.io/images/placeholders/1280x960.png" ~alt:"Placeholder image" ()
          ]
        ];
        H.div ~a:[H.a_class ["card-content"]] [
          H.div ~a:[H.a_class ["media"]] [
            H.div ~a:[H.a_class ["media-left"]] [
              H.figure ~a:[H.a_class ["image"; "is-48x48"]] [
                H.img ~src:"https://bulma.io/images/placeholders/96x96.png" ~alt:"Placeholder image" ()
              ]
            ];
            H.div ~a:[H.a_class ["media-content"]] [
              H.p ~a:[H.a_class ["title"; "is-4"]] [H.txt (Database.LocalUser.display_name user)];
              H.p ~a:[H.a_class ["subtitle"; "is-6"]] [H.txt "@"; H.txt (Database.LocalUser.username user)];
            ]
          ];
          H.div ~a:[H.a_class ["content"]] [
            Database.LocalUser.about user
            |> Option.value ~default:"Apparently, this person likes to keep an air of mystery about themselves"
            |> H.txt
          ];
          H.div ~a:[H.a_class ["level"]] [
            H.div ~a:[H.a_class ["level-item"]] [
              H.div ~a:[H.a_class ["container"]] [
                H.b [H.txt "Followers"];
                H.p [H.txt (Int.to_string followers)];
              ]
            ];
            H.div ~a:[H.a_class ["level-item"]] [
              H.div ~a:[H.a_class ["container"]] [
                H.b [H.txt "Following"]; 
                H.p [H.txt (Int.to_string following)];
              ]
            ];
            H.div ~a:[H.a_class ["level-item"]] [
              H.div ~a:[H.a_class ["container"]] [
                H.b [H.txt "Toasts"]; 
                H.p [H.txt (Int.to_string posts)];
              ]
            ]            
          ]
        ]
      ]
    ]
  ]

let build_url config user state time offset txt incr =
  let url = Configuration.Url.user_profile_page config
              (Database.LocalUser.username user)
            |> Fun.flip Uri.with_query [
              "state", [state];
              "time", 
              (Ptime.of_float_s
                 (CalendarLib.Calendar.to_unixfloat time))
              |> Option.map (Ptime.to_rfc3339 ~tz_offset_s:0)
              |> Option.to_list ;
              "offset", [Int.to_string (offset + incr)]]
            |> Uri.to_string in
  B.level [
    H.div ~a:[H.a_class ["level-item"]] [
      B.button ~a_class:["is-link"]
        ~a:[H.a_href url]
        txt
    ]
  ]

let build_pagination_controls config state user time offset data =
  [B.level @@ List.concat [
        (match offset with 0 -> [] | _ -> [
             H.div ~a:[H.a_class ["level-item"]] [
               build_url config user state time offset "prev" (-1)
             ]
           ]
        );
        (match data with [] -> [] |  _ :: _ -> [
             H.div ~a:[H.a_class ["level-item"]] [
               build_url config user state time offset "next" 1
             ]
           ]);
      ]]

let build_contents config user _req = function
  | `Following (time,offset,posts) ->
    B.container @@ List.concat [
      [B.container @@ List.map (Follow.build config) posts];
      build_pagination_controls config "following" user time offset posts
    ]
  | `Followers (time,offset,posts) ->
    B.container @@ List.concat [
      [B.container @@ List.map (Follow.build config) posts];
      build_pagination_controls config "followers" user time offset posts
    ]
  | `Posts (time,offset,posts) ->
    B.container @@ List.concat [
      [B.container @@ List.map (Post.build config) posts];
      build_pagination_controls config "post" user time offset posts
    ]

let body config ~state ~stats:(following,followers,posts)
      (current_user: Database.LocalUser.t option) (user: Database.LocalUser.t) req =
  H.body ~a:[H.a_class ["has-navbar-fixed-top"]] @@ List.concat [
    [Navbar.build current_user req];
    [build req (following,followers,posts) user];
    [(build_contents config user req state)];
    [Navbar.script];
    [noscript "Javascript may be required (but don't worry, it's all Libre my friend!)"]
  ]


let build config ~state ~posts ~following ~followers current_user user req =
  let head = Components.build_header ~title:(Database.LocalUser.display_name user) () in
  let body = body config ~state ~stats:(following,followers,posts) current_user user req in
  Utils.build_document head body
