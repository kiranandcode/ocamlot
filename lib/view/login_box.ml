open Utils

let render_login_box ?action ?(fields=[]) () =
  div "login-box" [
    Form.render_input_form ?action (List.concat [
        [Form.render_input_form_entry ~ty:`Text
           ~value:"username" ~name:"Username" ();
         Form.render_input_form_entry ~ty:`Password
           ~value:"password" ~name:"Password" ();
         Form.render_input_form_submit ~name:"Log in" ()];
        hidden_fields fields;
      ])
  ]

let render_register_box ?action ?(fields=[]) () =
  div "register-box" [
    Form.render_input_form ?action (List.concat  [[
        Form.render_input_form_entry ~ty:`Text
          ~value:"username" ~name:"Username" ();
        Form.render_input_form_entry ~ty:`Password
          ~value:"password" ~name:"Password" ();
        Form.render_input_form_entry ~ty:`Password
          ~value:"password2" ~name:"Password (again)" ();
        Form.render_input_form_entry ~ty:`Text
          ~value:"display-name" ~name:"Display Name" ();
        (* Form.render_input_form_checkbox *)
        (*   ~value:"manually-accepts-followers" *)
        (*   ~name:"Manually accepts followers" (); *)
        Form.render_input_form_textarea ~value:"about"
          ~name:"About" ();
        Form.render_input_form_submit ~name:"Sign up" ();
      ];
     hidden_fields fields
    ])
  ]

