module H = Tyxml.Html

let login_box () =
  H.div ~a:[H.a_class ["login-component"]] [
    H.div ~a:[H.a_class ["login-component-content"]] [
      Pure.form ~stacked:true [
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "username"] [H.txt "Username"];
          H.input ~a:[H.a_name "username"; H.a_input_type `Text] ();
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "password"] [H.txt "Password"];
          H.input ~a:[H.a_name "password"; H.a_input_type `Password] ();
        ];
        H.button ~a:[H.a_button_type `Submit] [H.txt "Log in"];
        Pure.a_button ~a:[H.a_href "./register.html"] [H.txt "Register"];
      ]
    ]
  ]

let register_box () =
  H.div ~a:[H.a_class ["login-component"]] [
    H.div ~a:[H.a_class ["login-component-content"]] [
      Pure.form ~stacked:true [
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "username"] [H.txt "Username"];
          H.input ~a:[H.a_name "username"; H.a_input_type `Text] ();
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "password"] [H.txt "Password"];
          H.input ~a:[H.a_name "password"; H.a_input_type `Password] ();
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "password"] [H.txt "Confirm Password"];
          H.input ~a:[H.a_name "password"; H.a_input_type `Password] ();
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "reason"] [H.txt "Notes"];
          H.input ~a:[H.a_name "reason"; H.a_input_type `Text] ();
        ];
        H.button ~a:[H.a_button_type `Submit] [H.txt "Sign up"];
        Pure.a_button ~a:[H.a_href "./login.html"] [H.txt "Log in"];
      ]
    ]
  ]
