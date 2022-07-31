
type core_type = Ppxlib.core_type
let pp_core_type = Ppxlib.Pprintast.core_type

type ret_ty =
    Unit
  | Tuple of {many: bool; tys: core_type list}
[@@deriving show]
type arg_ty =
  | Unit
  | Tuple of core_type list
[@@deriving show]

type ty = Arrow of arg_ty * ret_ty
[@@deriving show]
