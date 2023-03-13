open Utils

let render_settings_box ~fields ?registration_allowed () =
  Form.render_input_form ~id:"settings-form" ~action:"/settings" (List.concat [
    [Form.render_input_form_checkbox ?initial_value:registration_allowed ~value:"registration-allowed"
      ~name:"Registration allowed" ()];
    (* Form.render_input_form_textarea *)
    (*   ~name:"Welcome text" ~value:"welcome-text" () *)
    hidden_fields fields;
  ])
