open Utils

let render_write_post_box () =
  div "write-post-box" [
    Form.render_input_form [
      Form.render_input_form_entry ~a_class:"write-post-option"
        ~ty:`Text ~value:"title" ~name:"Title" ();
      Form.render_input_form_dropdown ~a_class:"write-post-option"
        ~value:"visibility" ~name:"Visibility" [
        None, "---SELECT---";
        Some "public", "Public";
        Some "followers", "Followers";
        Some "direct", "Direct";
      ];
      Form.render_input_form_dropdown ~a_class:"write-post-option"
        ~value:"content-type" ~name:"Content-type" [
        None, "---SELECT---";
        Some "markdown", "Markdown";
        Some "plain", "Plain";
        Some "org", "Org";
      ];
      Form.render_input_form_textarea ~value:"message" ~name:"Message" ()
    ]
  ]

let render_write_post_preview post =
  div "write-post-preview" [
    Post.render ~a_class:"post-large" post
  ]

