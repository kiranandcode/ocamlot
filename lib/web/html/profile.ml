open Common

let build _req user =
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
            H.txt "Lorem ipsum dolor sit amet, consectetur adipiscing \
                   elit. Phasellus nec iaculis mauris."
          ]
        ]
      ]
    ]
  ]


let body (current_user: Database.LocalUser.t option) (user: Database.LocalUser.t) req =
  H.body ~a:[H.a_class ["has-navbar-fixed-top"]] @@ List.concat [
    [Navbar.build current_user req];
    [build req user];
    [Navbar.script];
    [noscript "Javascript may be required (but don't worry, it's all Libre my friend!)"]
  ]


let build current_user user req =
  let head = Components.build_header ~title:"Home" () in
  let body = body current_user user req in
  Utils.build_document head body
