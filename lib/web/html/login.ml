module H = Tyxml.Html

let login_box ?(fields=[]) ?errors () =
  H.div ~a:[H.a_class ["login-component"]] @@ List.concat [
    begin match errors with
    | None -> []
    | Some errors ->
      [H.div ~a:[H.a_class ["login-component-errors"]]
         (List.map (fun error ->
            H.div ~a:[H.a_class ["login-error"]] [
              H.b [H.txt "Error: "];
              H.txt error
            ]
          ) errors)
      ]
    end;
    [H.div ~a:[H.a_class ["login-component-content"]] [
       Pure.form ~a:[H.a_method `Post] ~stacked:true @@ List.concat [
         [Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "username"] [H.txt "Username"];
             H.input ~a:[H.a_name "username"; H.a_input_type `Text] ();
           ];
          Pure.form_grouped_input [
            H.label ~a:[H.a_label_for "password"] [H.txt "Password"];
            H.input ~a:[H.a_name "password"; H.a_input_type `Password] ();
          ];];
         List.map (fun (key, value) ->
           H.input ~a:[H.a_name key; H.a_input_type `Hidden; H.a_value value] ()
         ) fields;
         [H.button ~a:[H.a_button_type `Submit] [H.txt "Log in"];
          Pure.a_button ~a:[H.a_href "/register"] [H.txt "Register"]]
       ]
    ]]
  ]

let register_box ?(fields=[]) ?errors () =
  H.div ~a:[H.a_class ["login-component"]] @@ List.concat [
    begin match errors with
    | None | Some [] -> []
    | Some errors ->
      [H.div ~a:[H.a_class ["login-component-errors"]]
         (List.map (fun error ->
            H.div ~a:[H.a_class ["login-error"]] [
              H.b [H.txt "Error: "];
              H.txt error
            ]
          ) errors)
      ]
    end;
    [H.div ~a:[H.a_class ["login-component-content"]] [
       Pure.form ~a:[H.a_method `Post; H.a_action "/register"] ~stacked:true @@ List.concat [
         [ Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "username"] [H.txt "Username"];
             H.input ~a:[H.a_name "username"; H.a_input_type `Text] ();
           ];
           Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "password"] [H.txt "Password"];
             H.input ~a:[H.a_name "password"; H.a_input_type `Password] ();
           ];
           Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "password2"] [H.txt "Confirm Password"];
             H.input ~a:[H.a_name "password2"; H.a_input_type `Password] ();
           ];
           Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "reason"] [H.txt "Notes"];
             H.input ~a:[H.a_name "reason"; H.a_input_type `Text] ();
           ]];
         List.map (fun (key, value) ->
           H.input ~a:[H.a_name key; H.a_input_type `Hidden; H.a_value value] ()
         ) fields;
         [H.button ~a:[H.a_button_type `Submit] [H.txt "Sign up"];
         Pure.a_button ~a:[H.a_href "/login"] [H.txt "Log in"]];
       ]
     ]]
  ]
