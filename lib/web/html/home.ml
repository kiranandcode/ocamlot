open Common

let build_post req =
  H.div ~a:[H.a_class ["hero"]] [
    H.div ~a:[H.a_class ["container"]] [
      H.form ~a:[H.a_method `Post] [
        Utils.csrf_tag req;
        H.div ~a:[H.a_class ["field"; "is-horizontal"]] [
          H.div ~a:[H.a_class ["field-label"; "is-normal"]] [
            H.label ~a:[H.a_class ["label"]; H.a_label_for "post"] [
              H.txt "Toast:"
            ]
          ];
          H.div ~a:[H.a_class ["field-body"]] [
            H.div ~a:[H.a_class ["field"]] [
              H.input ~a:[
                H.a_class ["input"];
                H.a_input_type `Text;
                H.a_placeholder "Post a toast to OCamlot!";
                H.a_name "post";
              ] ()
            ];
            H.input ~a:[H.a_class ["button"; "is-primary"]; H.a_input_type `Submit] ();
          ]
        ]
      ]
    ]
  ]


let body (user: Database.LocalUser.t option) req =
  H.body ~a:[H.a_class ["has-navbar-fixed-top"]] @@ List.concat [
    [Navbar.build user req];
    Option.map (fun _ -> [build_post req]) user
      |> Option.value ~default:[];
    [Navbar.script];
    [noscript "Javascript may be required (but don't worry, it's all Libre my friend!)"]
  ]


let build user req =
  let head = Components.build_header ~title:"Home" () in
  let body = body user req in
  Utils.build_document head body
