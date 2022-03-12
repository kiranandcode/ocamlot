open Common

let form req ~label ~field ~placeholder =
  H.form ~a:[H.a_method `Post] [
    Utils.csrf_tag req;
    H.div ~a:[H.a_class ["field"; "is-horizontal"]] [
      H.div ~a:[H.a_class ["field-label"; "is-normal"]] [
        H.label ~a:[H.a_class ["label"]; H.a_label_for field] [
          H.txt label
        ]
      ];
      H.div ~a:[H.a_class ["field-body"]] [
        H.div ~a:[H.a_class ["field"]] [
          H.input ~a:[
            H.a_class ["input"];
            H.a_input_type `Text;
            H.a_placeholder placeholder;
            H.a_name field;
          ] ()
        ];
        H.input ~a:[H.a_class ["button"; "is-primary"]; H.a_input_type `Submit] ();
      ]
    ]
  ]

let notification ?(classes=[]) elts =
  H.div ~a:[H.a_class ("notification" :: classes)]
    ((H.button ~a:[H.a_class ["delete"]] []) :: elts)

let build_post ?(errors=[]) req =
  H.div ~a:[H.a_class ["hero"]] [
    H.div ~a:[H.a_class ["container"; "hero-body"]] @@ List.concat [
      [H.h1 ~a:[H.a_class ["title"]] [H.txt "Welcome to OCamlot!"]];
      (List.map (fun err -> notification ~classes:["is-danger"; "is-light"] [H.txt err]) errors);
      [
        B.level [
          form req ~label:"Toast: " ~field:"post" ~placeholder:"Post a toast to OCamelot"
        ];
        B.level [
          form req ~label:"Follow: " ~field:"follow" ~placeholder:"username@domain.tld"
        ];
      ]]
  ]


let body ~errors (user: Database.LocalUser.t option) req =
  H.body ~a:[H.a_class ["has-navbar-fixed-top"]] @@ List.concat [
    [Navbar.build user req];
    Option.map (fun _ -> [build_post ~errors req]) user
      |> Option.value ~default:[];
    [Navbar.script];
    [noscript "Javascript may be required (but don't worry, it's all Libre my friend!)"]
  ]


let build ~errors user req =
  let head = Components.build_header ~title:"Home" () in
  let body = body ~errors user req in
  Utils.build_document head body
