open Containers
open Angstrom
open Ast
open Utils

let pragma_condition =
  choice [
    _ON *> return true;
    _OFF *> return false;    
  ]

let ty =
  choice [
    (* If the declared type contains the string "INT" then it is assigned INTEGER affinity. *)
    string_ci "integer" *> return INTEGER;
    string_ci "int" *> return INTEGER;
    (* If the declared type of the column contains any of the strings
       "CHAR", "CLOB", or "TEXT" then that column has TEXT
       affinity. Notice that the type VARCHAR contains the string
       "CHAR" and is thus assigned TEXT affinity. *)
    string_ci "char" *> return TEXT;
    string_ci "varchar" *> return TEXT;
    string_ci "text" *> return TEXT;
    (* If the declared type for a column contains the string "BLOB" or if no type is specified then the column has affinity BLOB. *)
    string_ci "blob" *> return BLOB;
    (* If the declared type for a column contains any of the strings "REAL", "FLOA", or "DOUB" then the column has REAL affinity. *)
    string_ci "real" *> return REAL;
    string_ci "float" *> return REAL;
    string_ci "double" *> return REAL;
    (* Otherwise, the affinity is NUMERIC. *)
    identifier >>= fun ty -> return (NUMERIC ty)
  ]

let column_constraint =
  let primary_key =
    let* _ = _PRIMARY *>>! _KEY in
    let* direction = choice [
      ws *> _ASC *> return (Some `ASC);
      ws *> _DESC *> return (Some `DESC);
      return None;
    ] in
    let* auto_increment = choice [
      ws *> _AUTOINCREMENT *> return true;
      return false;
    ] in
    return @@ (PRIMARY_KEY {direction; auto_increment}: column_constraint) in
  let unique = _UNIQUE *> return UNIQUE in
  let not_null = _NOT *>>! _NULL *> return NOT_NULL in
  choice [
    primary_key;
    not_null;
    unique;
  ]

let comment_spec =
  let* _ = string "/*" in
  fix (fun kont ->
    let* str = take_till (function '*' -> true | _ -> false) <* char '*' in
    let* term = String.of_char <$> any_char in
    match term with
    | "/" -> return str
    | c ->
      let* remain_str = kont in
      return @@ str ^ "*" ^ c ^ remain_str
  )


let column_def =
  let* name = identifier in
  let* ty = ws *> ty in
  let* constraints = ws *> sep_by ws column_constraint in
  let* info = choice [
    (ws *> comment_spec >>= fun s -> return (Some (String.trim s)));
    return None
  ] in
  return {name; ty; info; constraints}

let action =
  choice [
    _NO *>>! _ACTION *> return NO_ACTION;
    _RESTRICT *> return RESTRICT;
    _SET *>>! _NULL *> return SET_NULL;
    _SET *>>! _DEFAULT *> return SET_DEFAULT;
    _CASCADE *> return CASCADE;
  ]
let foreign_key_clause =
  choice [
    (_ON *>>! _DELETE *>>! action >>= fun t -> return (ON_DELETE t));
    (_ON *>>! _UPDATE *>>! action >>= fun t -> return (ON_UPDATE t));
  ]

let table_constraint =
  let primary_key =
    let* _ = _PRIMARY *>>! _KEY *>> char '(' in
    let* columns = ws *> sep_by1 (ws *> char ',' *> ws) identifier in
    let* _ = ws *> char ')' in
    return @@ PRIMARY_KEY columns in
  let foreign_key =
    let* key = _FOREIGN *>>! _KEY *>> char '(' *>> identifier <<* char ')' in
    let* references_table = ws *> _REFERENCES *>> identifier in
    let* references_key = ws *> char '(' *>> identifier <<* char ')' in
    let* triggers = ws *> sep_by ws foreign_key_clause in
    return @@ FOREIGN_KEY { key; references_table; references_key; triggers } in
  choice [
    primary_key;
    foreign_key;
  ]


let pragma =
  let* name = _PRAGMA *>>! identifier <<* char '=' in
  let* value = ws *> pragma_condition <<* char ';' in
  return @@ PRAGMA {name; value}

let create_table =
  let* _ = _CREATE *>>! _TABLE in
  let* if_not_exists =
    choice [
      ws *> _IF *>> _NOT *>> _EXISTS *> return true;
      return false
    ] in
  let* name = ws *> identifier in
  let* info = choice [
    ws *> comment_spec >>| Option.some;
    return None
  ] in
  let* _ = ws *> char '(' in
  let* columns = ws *> sep_by (ws *> char ',' *> ws) column_def in
  let* table_constraints =
    choice [
      ws *> char ',' *> ws *> sep_by ws table_constraint;
      return []
    ] in
  let* _ = ws *> char ')' in
  let* _ = ws *> char ';' in
  return @@ CREATE_TABLE {
    name; info;
    if_not_exists;
    columns;
    constraints=table_constraints
  }

let create_index =
  let* name = _CREATE *>>! _INDEX *>>! identifier <<* _ON in
  let* table = ws *> identifier <<* char '(' in
  let* key = ws *> sep_by1 (ws *> char ',' <* ws) identifier <<* char ')' in
  let* _ = ws *> char ';' in
  return @@ CREATE_INDEX {name; table; key}

let program =
  ws *> sep_by ws (choice [create_index; create_table; pragma]) <* ws
