module Types = Types
module Html = Brr_int.Html
open Types

let o_cons opt v = match opt with None -> [v] | Some ls -> v :: ls
let o_cat opt v = match opt with None -> v | Some ls -> v @ ls

module Bulma (H: HTML_SIG) = struct
  let txt = H.txt
  let a_class = H.a_class
  let a_class' v = H.a_class [v]


  let attrs_with_class a cls name attrs = o_cat a (a_class (o_cons cls name) :: attrs)

  let attrs_with_class_table a cls name attrs =
    o_cat a ((a_class (o_cons cls name)) :: attrs)

  let attrs_with_class_input a cls name attrs =
    o_cat a ((a_class (o_cons cls name)) :: attrs)


  let tag ?a_class:cls ?a txt = H.div ~a:(attrs_with_class a cls "tag" []) [H.txt txt]
  let tag_delete ?a_class:cls ?a txt =
    let delete = H.button ~a:([a_class ["delete"; "is-small"]]) [] in
    H.div ~a:(attrs_with_class a cls "tag" []) [txt; delete], delete

  let navbar ?a_class:cls ?a elts =
    H.nav ~a:(
      attrs_with_class a cls "navbar" [
        H.a_role ["navigation"];
        H.a_aria "label" ["main"; "navigation"]
      ]) elts

  let container ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "container" []) elts

  let columns ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "columns" []) elts

  let column ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "column" []) elts

  let navbar_brand ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "navbar-brand" []) elts

  let navbar_item ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "navbar-item" []) elts

  let navbar_item_a ?a_class:cls ?a elts =
    H.a ~a:(attrs_with_class a cls "navbar-item" []) elts

  let navbar_dropdown ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a (Some (o_cons cls "is-hoverable")) "navbar-dropdown" []) elts

  let navbar_link ?a_class:cls ?a elt =
    H.a ~a:(attrs_with_class a cls "navbar-link" []) elt

  let navbar_item_with_dropdown ?a_class:cls ?a elt elts =
    navbar_item ~a_class:(o_cat cls ["has-dropdown"; "is-hoverable"]) ?a [
      elt;
      navbar_dropdown elts
    ]

  let navbar_burger ?a_class:cls ?a:a' () =
    H.(a ~a:(attrs_with_class a' cls "navbar-burger" [
      a_role ["button"];
      a_aria "label" ["menu"];
      a_aria "expanded" ["false"]
    ]) (
      List.init 3 (fun _ -> span ~a:[a_aria "hidden" ["true"]] [])
    )
    )

  let navbar_start ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "navbar-start" []) elts

  let navbar_end ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "navbar-end" []) elts

  let navbar_menu ?a_class:cls ?a (stt,ed) =
    H.div ~a:(attrs_with_class a cls "navbar-menu" []) [
      navbar_start stt;
      navbar_end ed
    ]

  let panel ?a_class:cls ?a elts =
    H.nav ~a:(attrs_with_class a cls "panel" []) elts

  let panel_heading ?a_class:cls ?a elts =
    H.p ~a:(attrs_with_class a cls "panel-heading" []) elts

  let panel_block ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "panel-block" []) elts

  let panel_block_a ?a_class:cls ?a elts =
    H.a ~a:(attrs_with_class a cls "panel-block" []) elts

  let panel_tabs ?a_class:cls ?a elts =
    H.a ~a:(attrs_with_class a cls "panel-tabs" []) elts

  let button ?a_class:cls ?a text =
    H.a ~a:(attrs_with_class a cls "button" []) [H.txt text]

  let notification ?(delete=false) ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "notification" []) (
      (if delete then [
         H.button ~a:[a_class' "delete"] []]
       else []) @ elts
    )

  let input ?a_class:cls ?a () =
    H.input ~a:(attrs_with_class_input a cls "input" []) ()

  let table ?a_class:cls ?a elts =
    H.table ~a:(attrs_with_class_table a cls "table" []) elts

  let divider ?a_class:cls ?a text =
    H.div ~a:(attrs_with_class a cls "divider" []) [H.txt text]


  let block ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "block" []) elts

  let media ?a_class:cls ?a ?left ?right elts =
    H.div ~a:(attrs_with_class a cls "media" []) begin
      begin match left with None -> [] | Some left ->
        [H.figure ~a:[H.a_class ["media-left"]] left]
      end @ begin
        [H.div ~a:[H.a_class ["media-content"]] elts]
      end @ begin match right with None -> [] | Some right ->
        [H.div ~a:[H.a_class ["media-right"]] right]
      end
    end

  let control ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "control" []) elts

  let level ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "level" []) elts

  let field ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "field" []) elts

  let image ?a_class:cls ?a ~src ~alt () =
    H.p ~a:(attrs_with_class a cls "image" []) [H.img ~src ~alt ()]

  let image' ?a_class:cls ?a ~src ~alt () =
    let img = H.img ~src ~alt () in
    H.p ~a:(attrs_with_class a cls "image" []) [img],img

  let file_upload ?a_class:cls ?a () =
    H.div ~a:(attrs_with_class a cls "file" [])  [
      H.label ~a:[a_class' "file-label"] [
        H.input ~a:[a_class' "file-input"; H.a_input_type `File] ();
        H.span ~a:[a_class' "file-cta"] [
          H.span ~a:[a_class' "file-icon"] [
            H.i ~a:[a_class ["fas"; "fa-upload"]] [txt "📂"];
          ];
          H.span ~a:[a_class' "file-label"] [ txt "Choose a file..." ]
        ]
      ]
    ]


  let modal_close_button ?a_class:cls ?a () =
    H.button
      ~a:(attrs_with_class a cls "modal-close"
            [a_class ["is-large"]; H.a_aria "label" ["close"]])
      []

  let modal_background ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "modal-background" []) elts

  let modal_content ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "modal-content" []) elts

  let modal' ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "modal" []) elts

  let modal_card_head ?a_class:cls ?a title =
    let button =
      H.button
        ~a:([H.a_class [ "delete"];
             H.a_aria "label" ["close"]]) [] in      
    H.div ~a:(attrs_with_class a cls "modal-card-head" []) [
      H.p ~a:[H.a_class ["modal-card-title"]] [H.txt title];
      button
    ], button

  let modal_card_foot ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "modal-card-foot" []) elts

  let modal_card ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "modal-card" []) elts

  let modal_card_body ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "modal-card-body" []) elts

  let modal ?a_class:cls ?a ?background elts =
    let close_button = modal_close_button () in
    H.div ~a:(attrs_with_class a cls "modal" []) (List.flatten [
      (match background with None -> [] | Some background ->
         [H.div ~a:[a_class' "modal-background"] background]
      );
      [H.div ~a:[a_class' "modal-content"] elts];
      [close_button];
    ]), close_button

  let section ?a_class:cls ?a elts =
    H.div ~a:(attrs_with_class a cls "section" []) elts

  let section_title ?a_class:cls ?a elts =
    H.h2 ~a:(attrs_with_class a cls "title" []) elts

  let message ?a_class:cls ?a header body =
    H.article ~a:(attrs_with_class a cls "message" []) [
      H.div ~a:[H.a_class ["message-header"]] header;
      H.div ~a:[H.a_class ["message-body"]] body;
    ]

end

