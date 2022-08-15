module H = Tyxml.Html

let post_context context =
  [
    Pure.grid_row [ Pure.grid_col [
      H.div ~a:[H.a_class ["write-post-context"]] [
        H.h5 [H.txt "In reply to:"];
      ]
    ]];
    Pure.grid_row [Pure.grid_col [
      H.div ~a:[H.a_class ["write-post-context-item"]] [
        Feed.feed_item context
      ]
    ]]
  ]


let write_post_box () =
  H.div ~a:[H.a_class ["write-post-component"]] [
    H.div ~a:[H.a_class ["write-post-content"]] [
      Pure.form ~stacked:true [
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "title"] [H.txt "Title (optional)"];
          H.input ~a:[H.a_name "title"; H.a_input_type `Text] ();
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "to"] [H.txt "To (optional)"];
          H.input ~a:[H.a_placeholder "e.g. john@example.xyz, bob@temp.org, ...";
                      H.a_name "to"; H.a_input_type `Text] ();
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "content-type"] [H.txt "Content-type"];
          H.select ~a:[H.a_name "content-type"] [
            H.option ~a:[H.a_value "markdown"] (H.txt "Markdown");
            H.option ~a:[H.a_value "org"] (H.txt "Org-mode");
            H.option ~a:[H.a_value "text"] (H.txt "Text");
          ]
        ];
        Pure.form_grouped_input [
          H.label ~a:[H.a_label_for "contents"] [H.txt "Contents"];
          H.textarea ~a:[H.a_rows 10; H.a_name "contents"] (H.txt "");
        ];
        H.div ~a:[H.a_class ["preview"]] [
          H.button ~a:[H.a_button_type `Submit] [H.txt "Post"];
          Pure.a_button [H.txt "Preview"];
        ]
      ]
    ]
  ]
