[@@@warning "-30"]
type column_name = string option * string
[@@deriving show]

type sql_value =
  | STAR
  | C of column_name
  | HOLE of int
  | INT of int
  | BOOL of bool 
  | STRING of string
  | COALESCE of sql_value list
  | DATETIME of sql_value
  | COUNT of sql_value

and join_constraint = {
  table: string;
  table_name: string option;
  join_column: column_name;
  join_value: sql_value
} 

and select_query = {
  values: sql_value list;
  table: string;
  table_name: string option;
  join_constraint: join_constraint option;
  where_constraint: where_constraint option;
  order_by: sql_value option;
  order_direction: [`ASC | `DESC ] option;
  limit: sql_value option;
  offset: sql_value option;
}

and where_constraint =
  | TRUE
  | FALSE
  | EQ of column_name * sql_value
  | OR of where_constraint * where_constraint
  | AND of where_constraint * where_constraint
  | LEQ of sql_value * sql_value
  | LT of sql_value * sql_value
  | GEQ of sql_value * sql_value
  | GT of sql_value * sql_value
  | LIKE of sql_value * sql_value
  | IS_NOT_NULL of sql_value
  | EXISTS of bool * select_query


and query =
  | INSERT of {
    or_ignore: bool;
    table: string;
    columns: string list;
  }
  | UPDATE of {
      or_ignore: bool;
      table: string;
      set: (column_name * sql_value) list;
      where_constraint: where_constraint;
    }
  | DELETE of {
      table: string;
      where_constraint: where_constraint;
      limit: sql_value option;
    }
  | SELECT of select_query
[@@deriving show]
