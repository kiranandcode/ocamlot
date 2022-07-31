
let enc_ty ~loc (ty: Query_type.ty) =
  let ty_exp (ty: Ppxlib.core_type) =
    Builder.core_ty_to_caqti_repr ~loc ty in
  let tup2 x1 x2 = Ppxlib.([%expr Caqti_type.Std.tup2 [%e x1] [%e x2]]) in
  let tup3 x1 x2 x3 = Ppxlib.([%expr Caqti_type.Std.tup3 [%e x1] [%e x2] [%e x3]]) in
  let tup4 x1 x2 x3 x4 = Ppxlib.([%expr Caqti_type.Std.tup4 [%e x1] [%e x2] [%e x3] [%e x4]]) in
  let rec loop = function
    | [] -> assert false
    | x1 :: [] -> ty_exp x1
    | x1 :: x2 :: [] ->
      tup2 (ty_exp x1) (ty_exp x2)
    | x1 :: x2 :: x3 :: [] ->
      tup3 (ty_exp x1) (ty_exp x2) (ty_exp x3)
    | x1 :: x2 :: x3 :: x4 :: [] ->
      tup4 (ty_exp x1) (ty_exp x2) (ty_exp x3) (ty_exp x4)
    | x1 :: x2 :: x3 :: rest ->
      tup4 (ty_exp x1) (ty_exp x2) (ty_exp x3) (loop rest) in
  let encode_arg_ty = function
    | Query_type.Tuple []
    | Unit -> Ppxlib.([%expr Caqti_type.unit ])
    | Tuple args ->
      [%expr let open Caqti_type in [%e loop args]] in
  match ty with
  | Query_type.Arrow (arg_ty, Query_type.Unit) ->
    let arg_ty = encode_arg_ty arg_ty in
    Ppxlib.([%expr Caqti_request.Infix.( [%e arg_ty] -->. Caqti_type.unit ) ])
  | Query_type.Arrow (arg_ty, Query_type.Tuple { many=false; tys=ret_tys }) ->
    let arg_ty = encode_arg_ty arg_ty in
    let ret_tys = loop ret_tys in
    Ppxlib.([%expr let open Caqti_type in Caqti_request.Infix.( [%e arg_ty] -->! [%e ret_tys] ) ])    
  | Query_type.Arrow (arg_ty, Query_type.Tuple { many=true; tys=ret_tys }) ->
    let arg_ty = encode_arg_ty arg_ty in
    let ret_tys = loop ret_tys in
    Ppxlib.([%expr let open Caqti_type in Caqti_request.Infix.( [%e arg_ty] -->* [%e ret_tys] ) ])    
