open Containers
open Angstrom
open Query_ast
open Utils

let int =
  choice ~failure_msg:"expected integer 0|[1-9][0-9]*" [
    (let* c = String.of_char <$> satisfy is_digit_nz in
     let* cs = take_while is_digit in
     let s = (c ^ cs) in
     return s);
    String.of_char <$> satisfy is_digit;
  ] >>| int_of_string

let bool =
  choice ~failure_msg:"expected boolean true|false" [
    _TRUE >>| (fun _ -> true);
    _FALSE >>| (fun _ -> false);

  ]

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
