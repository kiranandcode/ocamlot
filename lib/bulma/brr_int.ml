open Brr
open Types

module Html = struct
  type 'a elt = El.t
  type 'a attrib = At.t list

  type ('a, 'b) nullary = ?a:'a attrib list -> unit -> 'b elt
  type ('a, 'b, 'c) unary = ?a:'a attrib list -> 'b elt -> 'c elt
  type ('a, 'b, 'c) star =
    ?a:'a attrib list -> 'b elt list -> 'c elt
  type uri = string

  let s = Jstr.of_string
  let a v = [v]

  let a_class : string list -> [> `Class ] attrib =
    fun ls -> List.map (fun v -> v |> Jstr.of_string |> At.class') ls

  let a_id : uri -> [> `Id ] attrib =
    fun id -> a (At.id (s id))

  let a_placeholder : uri -> [> `Placeholder ] attrib =
    fun id -> a (At.placeholder (s id))
  let a_href : uri -> [> `Href ] attrib = fun url -> a (At.href (s url))
  let a_mime_type : uri -> [> `Mime_type ] attrib = fun ty -> a (At.type' (s ty))
  let a_src : uri -> [> `Src ] attrib = fun src -> a (At.src (s src))
  let a_input_type : input_type -> [> `Input_Type ] attrib =
    fun v ->
    let i = match v with
      | `Url -> "url"
      | `Reset -> "reset"
      | `Number -> "number"
      | `Month -> "month"
      | `Button -> "button"
      | `Color -> "color"
      | `Email -> "email"
      | `Datetime_local -> "datetime-local"
      | `Date -> "date"
      | `Password -> "password"
      | `Time -> "time"
      | `Submit -> "submit"
      | `Datetime -> "datetime"
      | `Week -> "week"
      | `Tel -> "tel"
      | `Image -> "image"
      | `Search -> "search"
      | `Hidden -> "hidden"
      | `File -> "file"
      | `Range -> "range"
      | `Checkbox -> "checkbox"
      | `Text -> "text"
      | `Radio -> "radio" in
      a (At.type' (s i))

  let flatten = String.concat " "
  let a_value : uri -> [> `Value ] attrib =
    fun vl -> a (At.value (s vl))
  let a_readonly : unit -> [> `ReadOnly ] attrib = fun () ->
    a (At.true' (s "readonly"))
  let a_height : int -> [> `Height ] attrib = fun n -> a (At.height n)
  let a_width : int -> [> `Width ] attrib = fun n -> a (At.width n)
  let a_role : uri list -> [> `Role ] attrib = fun rls -> a (At.v (s "role") (s (flatten rls)))
  let a_aria : uri -> uri list -> [> `Aria ] attrib = fun tag vls -> a (At.v (s("aria-" ^ tag)) (s (flatten vls)))

  let txt : uri -> 'a txt elt = fun txt -> El.txt (s txt)

  let at = function None -> None | Some ls -> Some (List.flatten ls)

  let h1 : ( common ,  h_content_fun , _) star =
    fun ?a els -> let at = at a in El.h1 ?at els
  let h2 : ( common ,  h_content_fun , _) star =
    fun ?a els -> let at = at a in El.h2 ?at els
  let h3 : ( common ,  h_content_fun , _) star =
    fun ?a els -> let at = at a in El.h3 ?at els
  let h4 : ( common ,  h_content_fun , _) star =
    fun ?a els -> let at = at a in El.h4 ?at els
  let h5 : ( common ,  h_content_fun , _) star =
    fun ?a els -> let at = at a in El.h5 ?at els
  let h6 : ( common ,  h_content_fun , _) star =
    fun ?a els -> let at = at a in El.h6 ?at els

  let nav : ( nav_attrib ,  nav_content_fun , 'a nav) star =
    fun ?a els -> let at = at a in El.nav ?at els
  let p : ( p_attrib ,  p_content_fun , 'a p) star =
    fun ?a els -> let at = at a in El.p ?at els    
  let div : ( div_attrib ,  div_content_fun , 'a div ) star =
    fun ?a els -> let at = at a in El.div ?at els    

  let article : ( common ,  article_content_fun , [> `Article] ) star =
    fun ?a els -> let at = at a in El.article ?at els    

  let figure : ( figure_attrib ,  figure_content_fun , 'a figure) star =
    fun ?a els -> let at = at a in El.figure ?at els
  let i : (i_attrib , i_content_fun , i ) star =
    fun ?a els -> let at = at a in El.i ?at els
  let span : ( span_attrib , 'b span_content_fun , 'a span) star =
    fun ?a els -> let at = at a in El.span ?at els
  let a : (a_attrib , 'a,  ('a, 'b) a ) star =
    fun ?a els -> let at = at a in El.a ?at els
  let img : src:uri -> alt:uri -> ('b img_attrib , 'a img) nullary =
    fun ~src ~alt ?a elts ->
    let at = Option.value ~default:[] (at a) @ [ At.src (s src); At.v (s "alt") (s alt) ] in
    El.img ~at elts

  let canvas : (canvas_attrib , 'a, ('a,'b) canvas ) star =
    fun ?a els -> let at = at a in El.canvas ?at els

  let table : (table_attrib , table_content_fun , 'a table ) star =
    fun ?a els -> let at = at a in El.table ?at els

  let td : (td_attrib ,  td_content_fun , 'a td ) star =
    fun ?a els -> let at = at a in El.td ?at els

  let th : ( th_attrib ,  th_content_fun , 'a th ) star =
    fun ?a els -> let at = at a in El.th ?at els

  let tr : ( tr_attrib ,  tr_content_fun , 'a tr ) star =
    fun ?a els -> let at = at a in El.tr ?at els

  let label : ( label_attrib ,  label_content_fun , 'a label ) star =
    fun ?a els -> let at = at a in El.label ?at els

  let input : ( input_attrib , 'a input ) nullary =
    fun ?a els -> let at = at a in El.input ?at els

  let button : ( button_attrib ,  button_content_fun , 'a button ) star =
    fun ?a els -> let at = at a in El.button ?at els

  let tbody : (tbody_attrib , tbody_content_fun, 'a tbody) star =
    fun ?a els -> let at = at a in El.tbody ?at els

  let progress :
    (progress_attrib, progress_content_fun, [> `Progress ]) star =
    fun ?a els -> let at = at a in El.progress ?at els

end
(* module H = (Html : Types.HTML_SIG) *)
