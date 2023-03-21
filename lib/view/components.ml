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

let update_start ~start ~stop ~current =
  let max_dist = 15 in
  if stop - start < max_dist
  then start, stop
  else
    let start_diff = current - start in
    let stop_diff = stop - current in
    if stop_diff <= max_dist && start_diff <= max_dist
    then start,stop
    else
      let current_trim_stop = current + (15 - start_diff) in
      let current_trim_start = current - (15 - stop_diff) in
      if start_diff <= 15 && current_trim_stop <= stop
      then start, current_trim_stop
      else if stop_diff <= 15 && start <= current_trim_start
      then current_trim_start, stop
      else current - 7, current + 7

let render_pagination_numeric ?prev ?next ?current ~start ~stop url () =
  let start,stop = match current with None -> start,stop | Some current -> update_start ~start ~stop ~current in
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
  
