open Containers

type ty =
  | TEXT
  | NUMERIC of string
  | INTEGER
  | REAL
  | BLOB
[@@deriving show]

type column_constraint =
  | PRIMARY_KEY of {
    direction: [`ASC | `DESC] option;
    auto_increment: bool;
  }
  | UNIQUE
  | NOT_NULL
[@@deriving show]

type column = {
  name: string;
  ty: ty * string option;
  constraints: column_constraint list;
}
[@@deriving show]

type action =
  | NO_ACTION
  | RESTRICT
  | SET_NULL
  | SET_DEFAULT
  | CASCADE
[@@deriving show]

type foreign_key_clause =
  | ON_UPDATE of action
  | ON_DELETE of action
[@@deriving show]

type table_constraint =
  | FOREIGN_KEY of {
    key: string;
    references_table: string;
    references_key: string;
    triggers: foreign_key_clause list;
  }
  | PRIMARY_KEY of string list
[@@deriving show]

type stmt =
  | CREATE_INDEX of {
    name: string;
    table: string;
    key: string list;
  }
  | CREATE_TABLE of {
      name: string;
      if_not_exists: bool;
      columns: column list;
      constraints: table_constraint list;
    }
  | PRAGMA of { name: string; value: bool; }
[@@deriving show]

let file = "../../resources/schema.sql"
let schema = IO.with_in file IO.read_all

open Angstrom

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false
let is_digit_nz = function '1'..'9' -> true | _ -> false
let is_idrfst = function 'a'..'z'|'A'..'Z' | '_' -> true | _ -> false
let is_idrcnt = function 'a'..'z'|'A'..'Z' | '_' | '.' | '0'..'9' -> true | _ -> false

module StringSet = Set.Make (String)

let keywords = StringSet.of_list [
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

let _PRAGMA = string_ci "PRAGMA"
let _OFF = string_ci "OFF"
let _CREATE = string_ci "create"
let _TABLE = string_ci "table"
let _INDEX = string_ci "index"
let _IF = string_ci "if"
let _NOT = string_ci "not"
let _EXISTS = string_ci "exists"
let _PRIMARY = string_ci "primary"
let _FOREIGN = string_ci "foreign"
let _REFERENCES = string_ci "references"
let _KEY = string_ci "key"
let _NOT = string_ci "not"
let _NULL = string_ci "null"
let _UNIQUE = string_ci "unique"
let _ASC = string_ci "asc"
let _DESC = string_ci "desc"
let _AUTOINCREMENT = string_ci "autoincrement"
let _ON = string_ci "on"
let _DELETE = string_ci "delete"
let _UPDATE = string_ci "update"
let _SET = string_ci "set"
let _DEFAULT = string_ci "default"
let _CASCADE = string_ci "cascade"
let _RESTRICT = string_ci "restrict"
let _NO = string_ci "no"
let _ACTION = string_ci "action"

let lparen = char '('
let rparen = char ')'

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
let ( *>>! ) l r = l *> must_ws *> r

let identifier =
  let* c = String.of_char <$> satisfy is_idrfst in
  let* cs = take_while is_idrcnt in
  let s = (c ^ cs) in
  if StringSet.mem (String.lowercase_ascii s) keywords
  then fail "invalid identifier"
  else return s

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
  return {name; ty=(ty, info); constraints}

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
    name;
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


let () =
  match parse_string ~consume:Consume.All program schema with
  | Ok v ->
    print_endline @@ [%show: stmt list] v
  | Error msg ->
    failwith msg
