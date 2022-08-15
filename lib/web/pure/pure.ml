module H = Tyxml.Html
module Utils = Html_utils
open Html_utils

type grid_breakpoints = [
  | `sm
  | `md
  | `lg
  | `xl
  | `xxl
]
let grid_breakpoints_to_string = function
  | `sm -> "sm"
  | `md -> "md"
  | `lg -> "lg"
  | `xl -> "xl"
  | `xxl -> "xxl"

let grid_row ?a ?a_class elts =
  mdiv "pure-g" ?a ?a_class elts

let grid_col ?breakpoint ?(size=1) ?n ?a ?a_class elts =
  match n, breakpoint with
  | None, None -> mdiv (Format.sprintf "pure-u-%d" size) ?a ?a_class elts
  | None, Some br -> mdiv (Format.sprintf "pure-u-%s-%d" (grid_breakpoints_to_string br) size) ?a ?a_class elts
  | Some n, None -> mdiv (Format.sprintf "pure-u-%d-%d" size n) ?a ?a_class elts
  | Some n, Some br ->
    mdiv (Format.sprintf "pure-u-%s-%d-%d" (grid_breakpoints_to_string br) size n)
      ?a ?a_class elts

let grid_col_responsive ?a ?a_class ?n ?(size=1) breakpoints elts =
  let base = match n with
    | None -> Format.sprintf "pure-u-%d" size
    | Some n -> Format.sprintf "pure-u-%d-%d" size n in
  let breakpoints =
    List.map (fun (br, (sz, n)) ->
      Format.sprintf "pure-u-%s-%d-%d" (grid_breakpoints_to_string br) sz n
    ) breakpoints in
  H.div ?a:(H.a_class (base :: breakpoints @ !!a_class) +:: a) elts


let txt txt = H.p [H.txt txt]

let img ?a ?a_class ~src ~alt () =
  H.img ?a:(H.a_class ("pure-img" :: !!a_class) +:: a) ~src ~alt ()

let form ?(stacked=false) ?(aligned=false) ?a ?a_class elts =
  let a_class =
    match aligned, stacked with
    | true, _ -> "pure-form-aligned" +:: a_class
    | _, true -> "pure-form-stacked" +:: a_class
    | _, _ -> a_class in
  H.form ?a:(H.a_class ("pure-form" :: !!a_class) +:: a) elts

let form_control_group ?a ?a_class elts =
  mdiv "pure-control-group" ?a ?a_class elts

let form_checkbox ?a ?a_class ?input_a ?input_a_class elts =
  let input_a = match input_a_class with
      None -> input_a
    | Some clss -> H.a_class clss +:: input_a in
  H.label ?a:(H.a_class ("pure-checkbox" :: !!a_class) +:: a) (
    H.input ?a:(H.a_input_type `Checkbox +:: input_a) () :: elts
  )
let form_radio ?a ?a_class ?input_a ?input_a_class elts =
  let input_a = match input_a_class with
      None -> input_a
    | Some clss -> H.a_class clss +:: input_a in
  H.label ?a:(H.a_class ("pure-radio" :: !!a_class) +:: a) (
    H.input ?a:(H.a_input_type `Radio +:: input_a) () :: elts
  )

let form_grouped_input ?a ?a_class elts =
  H.fieldset ?a:(H.a_class ("pure-group" :: !! a_class) +:: a) elts

let a_button ?a ?a_class ?(disabled=false) ?(active=false) ?(primary=false) elts =
  let a_class = if disabled then "pure-button-disabled" +:: a_class else a_class in
  let a_class = if active then "pure-button-active" +:: a_class else a_class in
  let a_class = if primary then "pure-button-primary" +:: a_class else a_class in
  H.a ?a:(H.a_class ("pure-button" :: !! a_class) +:: a) elts
let button ?a ?a_class ?(disabled=false) ?(active=false) ?(primary=false) elts =
  let a_class = if disabled then "pure-button-disabled" +:: a_class else a_class in
  let a_class = if active then "pure-button-active" +:: a_class else a_class in
  let a_class = if primary then "pure-button-primary" +:: a_class else a_class in
  H.button ?a:(H.a_class ("pure-button" :: !! a_class) +:: a) elts
let button_group ?a ?a_class elts =
  mdiv "pure-button-group" ?a ?a_class elts

let menu ?a ?a_class ?(horizontal=false) ?(scrollable=false) ?(fixed=false) elts =
  let a_class = if horizontal then "pure-menu-horizontal" +:: a_class else a_class in
  let a_class = if scrollable then "pure-menu-scrollable" +:: a_class else a_class in
  let a_class = if fixed then "pure-menu-fixed" +:: a_class else a_class in
  mdiv "pure-menu" ?a ?a_class elts
let a_menu_heading ?a ?a_class elts =
  ma "pure-menu-heading" ?a ?a_class elts
let menu_heading ?a ?a_class elts =
  mspan "pure-menu-heading" ?a ?a_class elts
let menu_list ?a ?a_class elts =
  mul "pure-menu-list" ?a ?a_class elts
let menu_sublist ?a ?a_class elts =
  mul "pure-menu-children" ?a ?a_class elts
let menu_item
      ?a ?a_class
      ?(selected=false) ?(disabled=false) ?(has_children=false)
      elts =
  let a_class = if selected then "pure-menu-selected" +:: a_class else a_class in
  let a_class = if disabled then "pure-menu-disabled" +:: a_class else a_class in
  let a_class = if has_children then "pure-menu-has-children" +:: a_class else a_class in
  mli "pure-menu-list" ?a ?a_class elts
let menu_item_heading ?a ?a_class elts =
  mli "pure-menu-heading" ?a ?a_class elts
let menu_link ?a ?a_class elts =
  ma "pure-menu-link" ?a ?a_class elts
