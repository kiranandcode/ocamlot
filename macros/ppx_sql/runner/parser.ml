open Containers
open Angstrom
open Ast

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
let _NULL = string_ci "NULL" <* not_prefix

let int =
  choice ~failure_msg:"expected integer 0|[1-9][0-9]*" [
    (let* c = String.of_char <$> satisfy is_digit_nz in
     let* cs = take_while is_digit in
     let s = (c ^ cs) in
     return s);
    String.of_char <$> satisfy is_digit;
  ] >>| int_of_string

let identifier =
  let* c = String.of_char <$> satisfy is_idrfst in
  let* cs = take_while is_idrcnt in
  let s = (c ^ cs) in
  return s


let bool =
  choice ~failure_msg:"expected boolean true|false" [
    _TRUE >>| (fun _ -> true);
    _FALSE >>| (fun _ -> false);

  ]


let next_id =
  let c = ref 0 in
  fun () -> let vl = !c in incr c; vl

let column_name =
  choice ~failure_msg:"expected a column name - either table.column or column" [
    (
      let* table = identifier in
      let* _ = char '.' in
      let* column = identifier in
      return (Some table, column)
    );
    identifier >>| (fun s -> (None, s))
  ]
let column_name = column_name <?> "[column name]"

type dispatch = {
  where_constraint: dispatch -> where_constraint t;
  where_constraint_base: dispatch -> where_constraint t;
  select_query: dispatch -> select_query t;
}

let sql_value =
  fix (fun sql_value ->
    choice ~failure_msg:"expected a sql value" [
      char '(' *>> sql_value <<* char ')';
      char '*' >>| (fun _ -> STAR);
      char '?' >>| (fun _ -> HOLE (next_id ()));
      int >>| (fun v -> INT v);
      bool >>| (fun v -> BOOL v);
      _COALESCE *>> char '(' *>> sep_by1 (ws *> char ',' *> ws) sql_value <<* char ')' >>| (fun elts -> COALESCE elts) <?> "COALESCE";
      _DATETIME *>> char '(' *>> sql_value <<* char ')' >>| (fun elt -> DATETIME elt) <?> "DATETIME";
      _COUNT *>> char '(' *>> sql_value <<* char ')' >>| (fun elt -> COUNT elt) <?> "COUNT";
      column_name >>| (fun cn -> C cn) <?> "COLUMN NAME"
    ]
  )
let sql_value = sql_value <?> "[sql value]"

type comparison = [`LEQ | `GEQ | `LT | `GT ]

let where_constraint_base d =
  let value_comp =
    let* l = sql_value in
    let* op = ws *> choice ~failure_msg:"malformed where constraint - expected comparison or IS NOT NULL clause" [
      (string "<=" >>| fun _ -> `LEQ);
      (string ">=" >>| fun _ -> `GEQ);
      (string "<" >>| fun _ -> `LT);
      (string ">" >>| fun _ -> `GT);
      (_IS *> commit *>> _NOT *>> _NULL >>| fun _ -> `IS_NOT_NULL)
    ] in
    match op with
    | #comparison as op ->
      let* _ = commit in
      let* r = ws *> sql_value in
      return begin match op with
        | `GT -> GT (l,r)
        | `GEQ -> GEQ (l, r)
        | `LEQ -> LEQ (l, r)
        | `LT -> LT (l, r)
      end
    | `IS_NOT_NULL -> return (IS_NOT_NULL l) in
  choice ~failure_msg:"expected a basic where constraint - either a comparison or equality" [
    (let* _ = char '(' in
     let* res = ws *> (d.where_constraint d) <<* char ')' in
     return res );
    (_EXISTS *>> char '(' *>>>  lazy (d.select_query d) <<* char ')' >>| (fun q -> EXISTS q));
    (let* cn = column_name <<* char '=' in
     let* vl = ws *> sql_value in
     return (EQ (cn, vl))) <?> "WHERE.EXISTS";
    value_comp <?> "WHERE.COMPARISON";
    (_TRUE >>| fun _ -> TRUE);
    (_FALSE >>| fun _ -> FALSE);
  ]

let where_constraint d =
  let* l = d.where_constraint_base d in
  let* op =  choice [
    ws *> _AND >>| (fun _ -> Some `AND);
    ws *> _OR >>| (fun _ -> Some `OR);
    return None
  ] in
  match op with
  | None -> return l
  | Some op ->
    let* _ = commit in
    let* r = ws *> d.where_constraint d in
    return @@ match op with
    | `AND -> AND (l, r)
    | `OR -> OR (l, r)

let update_eq =
  let* name = column_name in
  let* _ = ws *> char '=' in
  let* vl = ws *> sql_value in
  return (name, vl)
let update_eq = update_eq <?> "[update eq]"

let join_constraint =
  let* table = _JOIN *>> identifier in
  let* table_name = choice [
    ws *> _AS *>> identifier >>| Option.some;
    return None
  ] <?> "JOIN.AS" in
  let* _ = ws *>  _ON in
  let* join_column = ws *> column_name  <<* char '=' in
  let* join_value = ws *> sql_value <?> "JOIN.VALUE" in
  return ({table; table_name; join_column; join_value}: join_constraint)
let join_constraint = join_constraint <?> "JOIN _ ON _"

let order_by =
  let* ob = 
    choice ~failure_msg:"expected an order by constraint here" [
      ws *> _ORDER *>> _BY *>> sql_value >>| Option.some;
      return None
    ] in
  match ob with
  | None -> return (None, None)
  | Some _ ->
    let* ob_dir = choice ~failure_msg:"expected either an ascending or descending query here" [
      (ws *> _DESC >>| fun _ -> Some `DESC);
      (ws *> _ASC >>| fun _ -> Some `ASC);
      return None
    ] in
    return (ob, ob_dir)
let order_by = order_by <?> "ORDER BY"

let select_query d =
  let* values = _SELECT *>> sep_by1 (ws *> char ',' *> ws) sql_value in
  let* table = ws *> _FROM *>> identifier <?> "SELECT.FROM" in
  let* _ = commit in
  let* table_name = choice [ws *> _AS *>> identifier >>| Option.some; return None] <?> "SELECT.AS" in
  let* join_constraint = choice [ws *> join_constraint >>| Option.some; return None] <?> "SELECT.JOIN" in
  let* where_constraint = choice [ws *> _WHERE *>>> lazy (d.where_constraint d) >>| Option.some; return None] <?> "SELECT.WHERE" in
  let* order_by, order_direction = order_by <?> "ORDER BY" in
  let* limit = choice [ws *> _LIMIT *>> sql_value >>| Option.some; return None ] <?> "LIMIT" in
  let* offset = choice [ws *> _OFFSET *>> sql_value >>| Option.some; return None ] <?> "OFFSET" in
  return {values; table; table_name; join_constraint; where_constraint; order_by; order_direction; limit; offset}

let d = {
  where_constraint;
  where_constraint_base;
  select_query
}

let where_constraint = where_constraint d <?> "WHERE [where constraint]"
let select_query = select_query d <?> "SELECT [select query]"

let insert_query =
  let* _ = _INSERT in
  (* let* _ = commit in *)
  let* or_ignore =
    choice [
      ws *> _OR *>> _IGNORE >>| (fun _ -> true);
      return false 
    ] <?> "INSERT.OR_IGNORE" in
  let* table = ws *> _INTO *>> identifier <<* char '(' <?> "INSERT.INTO" in
  let* columns = sep_by (ws *> char ',' *> ws) identifier <<* char ')' in
  (* let* _ = commit in *)
  let* values = ws *> _VALUES *>> char '(' *>> sep_by (ws *> char ',' *> ws) (char '?') <<* char ')' <?> "INSERT.VALUES" in
  if List.length values = List.length columns
  then return (INSERT {
    or_ignore;
    table;
    columns
  })
  else fail "values in INSERT clause did not match up with columns"

let update_query =
  let* _ = _UPDATE in
  (* let* _ = commit in *)
  let* or_ignore =
    choice [
      ws *> _OR *>> _IGNORE >>| (fun _ -> true);
      return false 
    ] <?> "UPDATE.OR_IGNORE" in
  let* table = ws *> identifier <<* _SET <?> "UPDATE.TABLE" in
  let* set = ws *> sep_by (ws *> char ',' *> ws) update_eq <<* _WHERE <?> "UPDATE.SET" in
  let* where_constraint = ws *> where_constraint <?> "UPDATE.WHERE" in
  return (UPDATE {
    or_ignore;
    table;
    set;
    where_constraint
  })

let delete_query =
  let* table = _DELETE *>> _FROM *>> identifier <?> "DELETE.FROM" in
  let* where_constraint = ws *> _WHERE *>> (where_constraint <?> "DELETE WHERE constraint") in
  let* limit = choice [ws *> _LIMIT *>> sql_value >>| Option.some; return None ] in
  return (DELETE {
    table;
    where_constraint;
    limit
  })

let select_query = select_query >>| fun s -> SELECT s

let query =
  ws *> choice ~failure_msg:"expected either an INSERT, UPDATE, DELETE or SELECT query" [
    insert_query <?> "INSERT query";
    update_query <?> "UPDATE query";
    select_query <?> "SELECT query";
    delete_query <?> "DELETE query";
  ] <* ws

(* let parse_query s =
 *   parse_string ~consume:Consume.All query (String.trim s) *)

let fail_to_string marks err =
  String.concat " > " marks ^ ": " ^ err

let state_to_verbose_result = function
  | Buffered.Partial _ -> Error "incomplete input"
  | Done (_, v) -> Ok v
  | Fail (unconsumed, marks, msg) ->
    let remaining_big_string = (Bigstringaf.sub unconsumed.buf ~off:unconsumed.off ~len:unconsumed.len) in
    let combined_msg = fail_to_string marks msg ^ "\nUnconsumed:\n" ^ Bigstringaf.to_string remaining_big_string in
    Error combined_msg

let parse_query s =
  let state = Buffered.parse query in
  let state = Buffered.feed state (`String s) in
  let state = Buffered.feed state `Eof in
  state_to_verbose_result state
