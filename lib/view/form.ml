open Utils

let optional f name = match name with None -> [] | Some vl -> [f vl]

let render_input_form_textarea ?(rows=5) ?(cols=40) ?initial_value ~value ~name () =
  div "input-form-entry" [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.textarea ~a:[ H.a_name value; H.a_rows rows; H.a_cols cols; ]
      (Option.value ~default:(H.txt "") initial_value)
  ]

let (^::) h t = match h with None -> t | Some h -> h :: t

let render_input_form_entry ?a_class ?(multiple=false)
    ?(disabled=false) ?initial_value ?initial_values ~ty ~value ~name () =
  H.div ~a:[H.a_class (a_class ^:: ["input-form-entry"])] [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.input ~a:(List.flatten [
        [H.a_input_type ty; H.a_name value; ];
        optional H.a_value initial_value;
        (match initial_values with Some ((_ :: _) as values) -> List.map H.a_value values | _ -> []);
        optional H.a_disabled (if disabled then Some () else None);
        optional H.a_multiple (if multiple then Some () else None)
      ]) ()
  ]

let render_input_form_dropdown ?selected ?a_class ~value ~name options =
  H.div ~a:[H.a_class (a_class ^:: ["input-form-entry"])] [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.select ~a:[H.a_name value] (List.map (fun (option, name) ->
        match option,selected with
          None, _ -> H.option (H.txt name)
        | Some option, Some selected when String.equal option selected ->
          H.option ~a:[H.a_value option; H.a_selected ()] (H.txt name)
        | Some option, _ -> H.option ~a:[H.a_value option] (H.txt name)
      ) options)
  ]


let render_input_form_submit ?(disabled=false) ~name () =
  div "input-form-single-entry" [
    H.input ~a:(List.flatten [
        [H.a_input_type `Submit; H.a_value name ];
        optional H.a_disabled (if disabled then Some () else None)
      ]) ()
  ]

let render_input_form_checkbox ?(disabled=false) ?(initial_value=false) ~value ~name () =
  div "input-form-entry" [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    div "input-form-entry-checkbox" [
      H.input ~a:(List.flatten [
          [H.a_input_type `Checkbox; H.a_name value; ];
          if initial_value then [H.a_checked ()] else [];
          optional H.a_disabled (if disabled then Some () else None)
        ]) ()
    ]
  ]

let render_input_form_one_line ?(fields=[]) ?(a_class=[]) ?(disabled=false)
    ?initial_value ~ty ~value ~name ~submit_name () =
  H.div ~a:[
    H.a_class ("input-form-entry" :: "input-form-entry-one-line" :: a_class)
  ] (List.concat [
      [
        H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
        H.input ~a:(List.flatten [
            [H.a_input_type ty; H.a_name value];
            optional H.a_value initial_value;
            optional H.a_disabled (if disabled then Some () else None)
          ]) ();
        H.input ~a:[H.a_input_type `Submit; H.a_value submit_name] ()
      ];
      List.map (fun (k,v) -> H.input ~a:[H.a_input_type `Hidden; H.a_name k; H.a_value v] ()) fields
    ])


let render_input_form ?id ?action ?enc_type elts =
  H.form ~a:(List.concat [
      [H.a_class ["input-form"]];
      (match action with None -> [] | Some action -> [H.a_action action; H.a_method `Post]);
      (match id with None -> [] | Some id -> [H.a_id id]);
      (match enc_type with | None -> [] | Some enc_type -> [H.a_enctype enc_type])      
    ]) elts
