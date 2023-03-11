module H = Tyxml.Html
open Utils

let render_pagination links =
  H.div ~a:[H.a_class ["flex"; "justify-center"]] [
    H.nav ~a:[H.a_class ["pagination-bar"]] 
      (List.map (fun link ->
           H.a ~a:[H.a_class ["pagination-item"]; H.a_href link.url]
             [H.txt link.text]
         ) links)
  ]

let render_pagination_numeric ?prev ?next ~start ~stop url () =
  let elements =
    List.init (stop - start)
      (fun i -> {url=url (start + i); text=string_of_int (start + i); form=None}) in
  let elements =
    (Option.map (fun url -> {url; text="<-"; form=None}) prev |> Option.to_list) @
    elements @
    (Option.map (fun url -> {url; text="->"; form=None}) next |> Option.to_list) in
  render_pagination elements

let render_heading_options options =
  match options with
  | [] -> []
  | options ->
    [div "elements-list" (
        List.map (fun link ->
            H.a ~a:[H.a_class ["feed-item"]; H.a_href link.url]
              [H.txt (link.text)]
          ) options
        |> intersperse (fun () -> H.p [H.txt "|"])
      )]

let render_heading_actions = function
  | [] -> []
  | actions -> [
      spacer ();
      H.div ~a:[H.a_class ["header-action-panel"; "elements-list"]]
        (List.map (fun link ->
           match link.form with
           | None -> H.a ~a:[H.a_href link.url] [H.txt link.text]
           | Some form ->
             H.input ~a:[H.a_form form; H.a_name link.url; H.a_value link.text; H.a_input_type `Submit] ()
           ) actions
         |> intersperse (fun () -> H.p [H.txt "|"]))
    ]

let render_heading ?(options=[]) ?(actions=[]) ~icon ~current  () =
  H.div ~a:[H.a_class ["header"; "flex"]] (List.concat [ [
      H.div ~a:[H.a_class ["number-icon"; "primary-border-small"]] [
        H.b [H.txt icon]
      ];
      div "header-details" (List.concat [
          [
            div "header-current-element" [
              div "bold" [ H.p [H.txt current]]
            ];

          ];
          render_heading_options options
        ]);
    ];
      render_heading_actions actions
    ])
  
