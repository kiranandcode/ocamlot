type ('a,'b) a = [> 'a Html_types.a] as 'b
type input_type = [ `Button | `Checkbox | `Color | `Date | `Datetime | `Datetime_local
                  | `Email | `File | `Hidden | `Image | `Month | `Number | `Password
                  | `Radio | `Range | `Reset | `Search | `Submit | `Tel | `Text | `Time | `Url | `Week ]

type common = Html_types.common
type nav_attrib = Html_types.nav_attrib
type 'a txt = [> Html_types.txt] as 'a
type 'a nav = [> Html_types.nav] as 'a
type p_attrib = Html_types.p_attrib
type 'a p = [> Html_types.p] as 'a
type h_content_fun = Html_types.h1_content_fun
type p_content_fun = Html_types.p_content_fun
type nav_content_fun = Html_types.nav_content_fun
type div_attrib = Html_types.div_attrib
type div_content_fun = Html_types.div_content_fun
type 'a div = [> Html_types.div] as 'a
type figure_attrib = Html_types.figure_attrib
type figure_content_fun = Html_types.figure_content_fun
type progress_attrib = Html_types.progress_attrib
type progress_content_fun = Html_types.progress_content_fun

type 'a figure = [> Html_types.figure] as 'a
type i_attrib = Html_types.i_attrib
type i_content_fun = Html_types.i_content_fun
type i = Html_types.i
type span_attrib = Html_types.span_attrib
type 'a span_content_fun = [< Html_types.span_content_fun] as 'a
type 'a span = [> Html_types.span] as 'a
type a_attrib = Html_types.a_attrib
type article_content_fun = Html_types.article_content_fun
type a_content_fun = Html_types.a_content_fun
type 'a img_attrib = [< Html_types.img_attrib] as 'a
type img_content_fun = Html_types.img_content_fun
type 'a img = [> Html_types.img] as 'a
type canvas_attrib = Html_types.canvas_attrib
type canvas_content_fun = Html_types.canvas_content_fun
type ('a, 'b) canvas = [> 'a Html_types.canvas] as 'b

type table_attrib = Html_types.table_attrib
type table_content_fun = Html_types.table_content_fun
type 'a table = [> Html_types.table] as 'a

type tbody_attrib = Html_types.tbody_attrib
type tbody_content_fun = Html_types.tbody_content_fun
type 'a tbody = [> Html_types.tbody] as 'a


type td_attrib = Html_types.td_attrib
type td_content_fun = Html_types.td_content_fun
type 'a td = [> Html_types.td] as 'a

type th_attrib = Html_types.th_attrib
type th_content_fun = Html_types.th_content_fun
type 'a th = [> Html_types.th] as 'a

type tr_attrib = Html_types.tr_attrib
type tr_content_fun = Html_types.tr_content_fun
type 'a tr = [> Html_types.tr] as 'a

type label_attrib = Html_types.label_attrib
type label_content_fun = Html_types.label_content_fun
type 'a label = [> Html_types.label] as 'a

type input_attrib = Html_types.input_attrib
type input_content_fun = Html_types.input_content_fun
type 'a input = [> Html_types.input] as 'a

type button_attrib = Html_types.button_attrib
type button_content_fun = Html_types.button_content_fun
type 'a button = [> Html_types.button] as 'a
(* Tyxml_html.article *)

module type HTML_SIG = sig
  type 'a elt
  type 'a attrib
  type ('a, 'b) nullary = ?a:'a attrib list -> unit -> 'b elt
  type ('a, 'b, 'c) unary = ?a:'a attrib list -> 'b elt -> 'c elt
  type ('a, 'b, 'c) star =
    ?a:'a attrib list -> 'b elt list -> 'c elt
  type uri = string

  val a_class : string list -> [> `Class ] attrib
  val a_id : uri -> [> `Id ] attrib
  val a_placeholder : uri -> [> `Placeholder ] attrib
  val a_href : uri -> [> `Href ] attrib
  val a_mime_type : uri -> [> `Mime_type ] attrib
  val a_src : uri -> [> `Src ] attrib
  val a_input_type : input_type ->
    [> `Input_Type ] attrib
  val a_value : uri -> [> `Value ] attrib
  val a_readonly : unit -> [> `ReadOnly ] attrib
  val a_height : int -> [> `Height ] attrib
  val a_width : int -> [> `Width ] attrib
  val a_role : uri list -> [> `Role ] attrib
  val a_aria : uri -> uri list -> [> `Aria ] attrib

  val h1: (common, h_content_fun , [> `H1 ]) star
  val h2: (common, h_content_fun , [> `H2 ]) star
  val h3: (common, h_content_fun , [> `H3 ]) star
  val h4: (common, h_content_fun , [> `H4 ]) star
  val h5: (common, h_content_fun , [> `H5 ]) star
  val h6: (common, h_content_fun , [> `H6 ]) star

  val txt : uri -> 'a txt elt
  val nav : ( nav_attrib ,  nav_content_fun , 'a nav) star
  val p : ( p_attrib ,  p_content_fun , 'a p) star
  val div : ( div_attrib ,  div_content_fun , 'a div ) star
  val figure : ( figure_attrib ,  figure_content_fun , 'a figure) star
  val i : (i_attrib , i_content_fun , i ) star
  val span : ( span_attrib , 'b span_content_fun , 'a span) star
  val a : (a_attrib , 'a,  ('a, 'b) a ) star
  val img : src:uri -> alt:uri -> ('b img_attrib , 'a img) nullary
  val canvas : (canvas_attrib , 'a, ('a,'b) canvas ) star

  val progress:
    (progress_attrib, progress_content_fun, [> `Progress ]) star

  val article : (common,  article_content_fun, [> `Article ]) star    

  val table : (table_attrib , table_content_fun , 'a table ) star
  val td : (td_attrib ,  td_content_fun , 'a td ) star
  val th : ( th_attrib ,  th_content_fun , 'a th ) star
  val tr : ( tr_attrib ,  tr_content_fun , 'a tr ) star
  val label : ( label_attrib ,  label_content_fun , 'a label ) star
  val input : ( input_attrib , 'a input ) nullary
  val button : ( button_attrib ,  button_content_fun , 'a button ) star
  val tbody: (tbody_attrib , tbody_content_fun, 'a tbody) star
end
