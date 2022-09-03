module H = Tyxml.Html

let write_post_title () =
  Pure.grid_row [ Pure.grid_col [
    H.div ~a:[H.a_class ["write-post-title"]] [
      H.h1 [H.txt "Write a new post"];
    ];
  ]]

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


let input_opt old_value ?(a=[]) () =
  match old_value with
  | None -> H.input ~a ()
  | Some old_value -> H.input ~a:(a @ [H.a_value old_value]) ()

let write_post_box  ?(fields=[]) ?errors ?title ?to_ ?(contents="") () =
  H.div ~a:[H.a_class ["write-post-component"]] @@ List.concat [
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
    [H.div ~a:[H.a_class ["write-post-content"]] [
       Pure.form ~a:[H.a_method `Post; H.a_action "/write"] ~stacked:true @@ List.concat [
         [
           Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "title"] [H.txt "Title (optional)"];
             input_opt title ~a:[H.a_name "title"; H.a_input_type `Text; ] ();
           ];
           Pure.form_grouped_input [
             H.label ~a:[H.a_label_for "to"] [H.txt "To (optional)"];
             input_opt to_ ~a:[H.a_placeholder "e.g. john@example.xyz, bob@temp.org, ...";
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
             H.textarea ~a:[H.a_rows 10; H.a_name "contents"] (H.txt contents);
           ];
           H.div ~a:[H.a_class ["write-post-panel"]] [
             H.input ~a:[
               H.a_class ["pure-button"];
               H.a_name "post-button";
               H.a_value "Post";
               H.a_input_type `Submit
             ] ();
             H.input ~a:[
               H.a_class ["pure-button"];
               H.a_name "preview-button";
               H.a_value "Preview";
               H.a_input_type `Submit
             ] ();
           ]
         ];
         List.map (fun (key, value) ->
           H.input ~a:[H.a_name key; H.a_input_type `Hidden; H.a_value value] ()
         ) fields;
       ];
     ]]
  ]
