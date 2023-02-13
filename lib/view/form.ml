open Utils

let optional f name = match name with None -> [] | Some vl -> [f vl]

let render_input_form_textarea ?(rows=5) ?(cols=40) ?initial_value ~value ~name () =
  div "input-form-entry" [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.textarea ~a:[ H.a_name value; H.a_rows rows; H.a_cols cols; ]
      (Option.value ~default:(H.txt "") initial_value)
  ]

let (^::) h t = match h with None -> t | Some h -> h :: t

let render_input_form_entry ?a_class
    ?(disabled=false) ?initial_value ~ty ~value ~name () =
  H.div ~a:[H.a_class (a_class ^:: ["input-form-entry"])] [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.input ~a:(List.flatten [
        [H.a_input_type ty; H.a_name value; ];
        optional H.a_value initial_value;
        optional H.a_disabled (if disabled then Some () else None)
      ]) ()
  ]

let render_input_form_dropdown ?a_class ~value ~name options =
  H.div ~a:[H.a_class (a_class ^:: ["input-form-entry"])] [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.select (List.map (fun (option, name) ->
        match option with
          None -> H.option (H.txt name)
        | Some option -> H.option ~a:[H.a_value option] (H.txt name)
      ) options)
  ]


let render_input_form_submit ?(disabled=false) ~name () =
  div "input-form-single-entry" [
    H.input ~a:(List.flatten [
        [H.a_input_type `Submit; H.a_value name ];
        optional H.a_disabled (if disabled then Some () else None)
      ]) ()
  ]

let render_input_form_checkbox ?(disabled=false) ?initial_value ~value ~name () =
  div "input-form-entry" [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    div "input-form-entry-checkbox" [
      H.input ~a:(List.flatten [
          [H.a_input_type `Checkbox; H.a_name value; ];
          optional H.a_value initial_value;
          optional H.a_disabled (if disabled then Some () else None)
        ]) ()
    ]
  ]

let render_input_form_one_line ?(a_class=[]) ?(disabled=false)
    ?initial_value ~ty ~value ~name ~submit_name () =
  H.div ~a:[
    H.a_class ("input-form-entry" :: "input-form-entry-one-line" :: a_class)
  ] [
    H.label ~a:[H.a_label_for value] [H.txt (name ^ ":")];
    H.input ~a:(List.flatten [
        [H.a_input_type ty; H.a_name value];
        optional H.a_value initial_value;
        optional H.a_disabled (if disabled then Some () else None)
      ]) ();
    H.input ~a:[H.a_input_type `Submit; H.a_value submit_name] ()
  ]


let render_input_form elts =
  H.form ~a:[H.a_class ["input-form"]] elts
