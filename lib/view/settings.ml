
let render_settings_box () =
  Form.render_input_form [
    Form.render_input_form_checkbox ~value:"registration"
      ~name:"Registration allowed" ();
    Form.render_input_form_textarea
      ~name:"Welcome text" ~value:"welcome-text" ()
  ]
