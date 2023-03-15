open Utils

let render_write_post_box ?(fields=[]) ?id ?action
      ?visibility ?attachments ?content_type ?title ?to_ ?message () =
  div "write-post-box" [
    Form.render_input_form ?id ?action ~enc_type:"multipart/form-data" (List.concat [[

      Form.render_input_form_entry ~a_class:"write-post-option"
        ~ty:`Text ~value:"title" ~name:"Title" ?initial_value:title ();

      Form.render_input_form_entry ~a_class:"write-post-option"
        ~ty:`Text ~value:"to" ~name:"To" ?initial_value:to_ ();

      Form.render_input_form_dropdown ?selected:visibility ~a_class:"write-post-option"
        ~value:"visibility" ~name:"Visibility" [
        (* None, "---SELECT---"; *)
        Some "public", "Public";
        Some "followers", "Followers";
        Some "direct-message", "Direct";
      ];

      Form.render_input_form_dropdown ~a_class:"write-post-option"
        ~value:"content-type" ~name:"Content-type" ?selected:content_type [
        (* None, "---SELECT---"; *)
        Some "markdown", "Markdown";
        Some "plain", "Plain";
        (* Some "org", "Org"; *)
      ];
      Form.render_input_form_entry ?initial_values:attachments ~ty:`File ~value:"attachment" ~name:"Attachments" ~multiple:true ();

      Form.render_input_form_textarea ~value:"message" ~name:"Message"
        ?initial_value:(Option.map H.txt message) ();
    ];
     hidden_fields fields
    ])
  ]

let render_write_post_preview post =
  div "write-post-preview" [
    Post.render ~a_class:"post-large" post
  ]

