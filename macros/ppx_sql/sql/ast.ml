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
  ty: ty;
  constraints: column_constraint list;
  info: string option
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
      name: string; info: string option;
      if_not_exists: bool;
      columns: column list;
      constraints: table_constraint list;
    }
  | PRAGMA of { name: string; value: bool; }
[@@deriving show]
