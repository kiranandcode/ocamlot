type cty = Ppxlib.core_type
let pp_cty = Ppxlib_ast.Pprintast.core_type

type column = {
  name: string;
  mapped_name: string option;
  is_unique: bool;
  ty: cty;
}
[@@deriving show]

type table = {
  name: string; ty: cty option;
  primary_key: string list option;
  columns: column list;
  foreign_keys: (string * string * string) list;
}
[@@deriving show]
