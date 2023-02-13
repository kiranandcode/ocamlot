open Utils


let render_header_option option =
  div "header-option" [
    H.a ~a:[H.a_href option.url] [H.txt option.text]
  ]

let render_header options =
  H.header [
    div_flex [
      div "header-option" [
        H.a ~a:[H.a_class ["bold"; "icon"]] [H.txt "OCamlot"]
      ]
    ];
    spacer ();
    div_flex 
      (List.map render_header_option options)
  ]
    
