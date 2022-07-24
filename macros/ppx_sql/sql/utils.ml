open Containers
open Angstrom

module StringSet = Set.Make (String)

let should_log = ref false


let id = ref 0
let next_id = fun () -> let vl = !id in incr id; vl
let with_fresh_ids f =
  let saved_id = !id in
  id := 0;
  let res = f () in
  let max_id = !id in
  id := saved_id;
  res, max_id


let printf s =
  Format.ksprintf ~f:(fun s ->
    if !should_log then  print_endline s else ()
  ) s

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false
let is_alpha = function 'a'..'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false
let is_digit_nz = function '1'..'9' -> true | _ -> false
let is_idrfst = function 'a'..'z'|'A'..'Z' | '_' -> true | _ -> false
let is_idrcnt = function 'a'..'z'|'A'..'Z' | '_' | '0'..'9' -> true | _ -> false

let ws =
  fix (fun ws ->
    choice [
      satisfy is_whitespace *> skip_while is_whitespace *> ws;
      string "--" *> take_till (function '\n' -> true | _ -> false) *> char '\n' *> ws;
      return ""
    ]
  )

let must_ws = satisfy is_whitespace *> ws

let ( <<* ) l r = l <* ws <* r
let ( !<<* ) l r = l <* must_ws <* r
let ( *>> ) l r = l *> ws *> r
let ( *>>> ) l r =
  let* _ = l *> ws in
  let (lazy r) = r in
  r
let ( *>>! ) l r = l *> must_ws *> r

let not_prefix =
  let* c = peek_char in
  if Option.exists is_alpha c
  then fail "keyword partial match"
  else return ()



let _INSERT = string_ci "insert" <* not_prefix
let _OR = string_ci "or" <* not_prefix
let _IGNORE = string_ci "ignore" <* not_prefix
let _INTO = string_ci "into" <* not_prefix
let _VALUES = string_ci "values" <* not_prefix
let _SELECT = string_ci "select" <* not_prefix
let _FROM = string_ci "from" <* not_prefix
let _WHERE = string_ci "where" <* not_prefix
let _UPDATE = string_ci "update" <* not_prefix
let _SET = string_ci "set" <* not_prefix
let _DATETIME = string_ci "datetime" <* not_prefix
let _COALESCE = string_ci "coalesce" <* not_prefix
let _AS = string_ci "as" <* not_prefix
let _ASC = string_ci "asc" <* not_prefix
let _DESC = string_ci "desc" <* not_prefix
let _ORDER = string_ci "order" <* not_prefix
let _BY = string_ci "by" <* not_prefix
let _LIMIT = string_ci "limit" <* not_prefix
let _OFFSET = string_ci "offset" <* not_prefix
let _OR = string_ci "or" <* not_prefix
let _AND = string_ci "and" <* not_prefix
let _TRUE = string_ci "true" <* not_prefix
let _FALSE = string_ci "false" <* not_prefix
let _COUNT = string_ci "count" <* not_prefix
let _DELETE = string_ci "delete" <* not_prefix
let _JOIN = string_ci "join" <* not_prefix
let _ON = string_ci "on" <* not_prefix
let _EXISTS = string_ci "exists" <* not_prefix
let _IS = string_ci "is" <* not_prefix
let _NOT = string_ci "not" <* not_prefix
let _NULL = string_ci "null" <* not_prefix

let _PRAGMA = string_ci "pragma" <* not_prefix
let _OFF = string_ci "off" <* not_prefix
let _CREATE = string_ci "create" <* not_prefix
let _TABLE = string_ci "table" <* not_prefix
let _INDEX = string_ci "index" <* not_prefix
let _IF = string_ci "if" <* not_prefix
let _PRIMARY = string_ci "primary" <* not_prefix
let _FOREIGN = string_ci "foreign" <* not_prefix
let _REFERENCES = string_ci "references" <* not_prefix
let _KEY = string_ci "key" <* not_prefix
let _UNIQUE = string_ci "unique" <* not_prefix
let _ASC = string_ci "asc" <* not_prefix
let _DESC = string_ci "desc" <* not_prefix
let _AUTOINCREMENT = string_ci "autoincrement" <* not_prefix
let _UPDATE = string_ci "update" <* not_prefix
let _DEFAULT = string_ci "default" <* not_prefix
let _CASCADE = string_ci "cascade" <* not_prefix
let _RESTRICT = string_ci "restrict" <* not_prefix
let _NO = string_ci "no" <* not_prefix
let _ACTION = string_ci "action" <* not_prefix

let keywords = StringSet.of_list [
  "null";
  "not";
  "is";
  "exists";
  "on";
  "join";
  "delete";
  "count";
  "false";
  "true";
  "and";
  "or";
  "offset";
  "limit";
  "by";
  "order";
  "desc";
  "asc";
  "as";
  "coalesce";
  "datetime";
  "set";
  "update";
  "where";
  "from";
  "select";
  "values";
  "into";
  "ignore";
  "or";
  "insert";
  "pragma";
  "off";
  "create";
  "table";
  "index";
  "if";
  "not";
  "exists";
  "primary";
  "foreign";
  "references";
  "key";
  "not";
  "null";
  "unique";
  "asc";
  "desc";
  "autoincrement";
  "on";
  "delete";
  "update";
  "set";
  "default";
  "cascade";
  "restrict";
  "no";
  "action";
]


let identifier =
  let* c = String.of_char <$> satisfy is_idrfst in
  let* cs = take_while is_idrcnt in
  let s = (c ^ cs) in
  if StringSet.mem (String.lowercase_ascii s) keywords
  then fail "invalid identifier"
  else return s

let fail_to_string marks err =
  String.concat " > " marks ^ ": " ^ err

let state_to_verbose_result =
  let open Angstrom in
  function
  | Buffered.Partial _ -> Error "incomplete input"
  | Done (_, v) -> Ok v
  | Fail (unconsumed, marks, msg) ->
    let remaining_big_string = (Bigstringaf.sub unconsumed.buf ~off:unconsumed.off ~len:unconsumed.len) in
    let combined_msg = fail_to_string marks msg ^ "\nUnconsumed:\n" ^ Bigstringaf.to_string remaining_big_string in
    Error combined_msg

let parse_safe query s =
  let open Angstrom in
  let state = Buffered.parse query in
  let state = Buffered.feed state (`String s) in
  let state = Buffered.feed state `Eof in
  state_to_verbose_result state
