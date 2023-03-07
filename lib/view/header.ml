open Utils

let render_header_option option =
  div "header-option" [
    H.a ~a:[H.a_href option.url] [H.txt option.text]
  ]

let render_header_option_submit option =
  div "header-option" [
    H.form ~a:[H.a_action option.url; H.a_method `Post] [
      H.input ~a:[H.a_input_type `Submit; H.a_value option.text] ()
    ]
  ]

let render_header ?action options =
  H.header [
    div_flex [
      div "header-option" [
        H.a ~a:[H.a_class ["bold"; "icon"]] [H.txt "OCamlot"]
      ]
    ];
    spacer ();
    div_flex (
      List.map render_header_option options @
      (Option.map render_header_option_submit action
       |> function Some v -> [v] | None -> [])
    )
  ]
   
