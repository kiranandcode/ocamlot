open Common

let field elts = H.div ~a:[H.a_class ["field"]] elts
let control elts = H.div ~a:[H.a_class ["control"]] elts

let label ~for_ text = H.label ~a:[H.a_class ["label"]; H.a_label_for for_] [H.txt text]
let text_input ~name () = H.input ~a:[H.a_class ["input"]; H.a_name name; H.a_input_type `Text] ()
let password_input ~name () = H.input ~a:[H.a_class ["input"]; H.a_name name; H.a_input_type `Password] ()

let notification ?(classes=[]) elts =
  H.div ~a:[H.a_class ("notification" :: classes)]
    ((H.button ~a:[H.a_class ["delete"]] []) :: elts)

let body ?(errors=[]) req =
  H.body [
    H.div ~a:[H.a_class ["hero"]] [
      H.div ~a:[H.a_class ["container"; "hero-body"]] @@ List.concat [
        [H.h1 ~a:[H.a_class ["title"]] [H.txt "Login to OCamlot"]];
        (List.map (fun err -> notification ~classes:["is-danger"; "is-light"] [H.txt err]) errors);
        [H.form ~a:[H.a_action "/login"; H.a_method `Post] [
          Utils.csrf_tag req;
          field [
            label ~for_:"username" "Username:";
            control [ text_input ~name:"username" () ]
          ];
          field [
            label ~for_:"password" "Password:";
            control [ password_input ~name:"password" () ]
          ];
          field [ control [
            H.input ~a:[H.a_class ["button"; "is-link"];  H.a_value "Login"; H.a_input_type `Submit] ();
            H.a ~a:[H.a_class ["button"; "is-light"]; H.a_href "/register"] [H.txt "Sign up"]
          ] ]
        ]];
      ]
    ];
    inline_script {|
document.addEventListener('DOMContentLoaded', () => {
  (document.querySelectorAll('.notification .delete') || []).forEach(($delete) => {
    const $notification = $delete.parentNode;

    $delete.addEventListener('click', () => {
      $notification.parentNode.removeChild($notification);
    });
  });
});
|};
    noscript "Javascript may be required (but don't worry, it's all Libre my friend!)"
  ]

let build ?errors req =
  let head = Components.build_header ~title:"Login to OCamlot" () in
  let body = body ?errors req in
  Utils.build_document head body
