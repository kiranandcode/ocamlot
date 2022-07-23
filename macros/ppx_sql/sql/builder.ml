open Containers

let build_type_from_schema ~loc name (table: Types.table) : Ppxlib.type_declaration =
  let build_field_decl (col: Types.column) : Ppxlib.label_declaration =
    Ppxlib.Ast_builder.Default.label_declaration
      ~loc ~name:{txt=Option.value
                        ~default:col.name
                        col.mapped_name; loc}
      ~mutable_:Immutable ~type_:col.ty in
  Ppxlib.Ast_builder.Default.type_declaration ~loc ~name
    ~params:[] ~cstrs:[]
    ~private_:Public ~manifest:None
    ~kind:(Ptype_record (List.map build_field_decl table.columns))


let caqti_type ~loc t =
  Ppxlib.Ast_builder.Default.(ptyp_constr
                                ~loc {txt=Longident.(Ldot (Lident "Caqti_type", "t")); loc} [
                                ptyp_constr ~loc {txt=Longident.Lident t; loc} []
                              ])

let build_enc_pattern ~loc (table: Types.table) =
  Ppxlib.Ast_builder.Default.(ppat_record ~loc
                                (List.map
                                   (fun (col: Types.column) ->
                                      let col_name = Option.value ~default:col.name col.mapped_name in
                                      ({txt=Longident.Lident col_name; loc}: _ Location.loc),
                                      ppat_var ~loc {txt=col_name; loc}
                                   )
                                   table.columns)
                                Closed)

let build_enc_expr ~loc (table: Types.table) =
  let tup_exp elts =
    Ppxlib.Ast_builder.Default.pexp_tuple ~loc elts in
  let col_exp (col: Types.column) =
    let col_name = Option.value ~default:col.name col.mapped_name in
    Ppxlib.Ast_builder.Default.pexp_ident ~loc
      Location.{txt=Lident col_name; loc} in
  let rec loop = function
    | [] -> assert false
    | x1 :: [] -> col_exp x1
    | x1 :: x2 :: [] -> tup_exp [col_exp x1; col_exp x2]
    | x1 :: x2 :: x3 :: [] -> tup_exp [col_exp x1; col_exp x2; col_exp x3]
    | x1 :: x2 :: x3 :: x4 :: [] -> tup_exp [col_exp x1; col_exp x2; col_exp x3; col_exp x4]
    | x1 :: x2 :: x3 :: rest ->
      tup_exp [col_exp x1; col_exp x2; col_exp x3; loop rest] in
  loop table.columns

let build_dec_pattern ~loc (table: Types.table) =
  let tup_pat elts =
    Ppxlib.Ast_builder.Default.ppat_tuple ~loc elts in
  let col_pat (col: Types.column) =
    let col_name = Option.value ~default:col.name col.mapped_name in
    Ppxlib.Ast_builder.Default.ppat_var ~loc
      Location.{txt=col_name; loc} in
  let rec loop = function
    | [] -> assert false
    | x1 :: [] -> col_pat x1
    | x1 :: x2 :: [] -> tup_pat [col_pat x1; col_pat x2]
    | x1 :: x2 :: x3 :: [] -> tup_pat [col_pat x1; col_pat x2; col_pat x3]
    | x1 :: x2 :: x3 :: x4 :: [] -> tup_pat [col_pat x1; col_pat x2; col_pat x3; col_pat x4]
    | x1 :: x2 :: x3 :: rest ->
      tup_pat [col_pat x1; col_pat x2; col_pat x3; loop rest] in
  loop table.columns

let build_dec_expr ~loc (table: Types.table) =
  Ppxlib.Ast_builder.Default.(pexp_record ~loc
                                (List.map
                                   (fun (col: Types.column) ->
                                      let col_name = Option.value ~default:col.name col.mapped_name in
                                      ({txt=Longident.Lident col_name; loc}: _ Location.loc),
                                      pexp_ident ~loc {txt=Lident col_name; loc})
                                   table.columns)
                                None)

let rec core_ty_to_caqti_repr ~loc (ct: Ppxlib.core_type) =
  match ct with
  | { ptyp_desc = Ppxlib.Ptyp_constr (enc, []); _ } ->
    Ppxlib.Ast_builder.Default.pexp_ident ~loc enc
  | { ptyp_desc = Ppxlib.Ptyp_constr (enc, args); _ } ->
    Ppxlib.Ast_builder.Default.pexp_apply ~loc (Ppxlib.Ast_builder.Default.pexp_ident ~loc enc)
      (List.map (fun arg -> Ppxlib.Nolabel, core_ty_to_caqti_repr ~loc arg) args)
  | _ -> failwith "invalid assumptions"

let build_caqti_repr ~loc (table: Types.table) =
  let tup2 x1 x2 = Ppxlib.([%expr Caqti_type.Std.tup2 [%e x1] [%e x2]]) in
  let tup3 x1 x2 x3 = Ppxlib.([%expr Caqti_type.Std.tup3 [%e x1] [%e x2] [%e x3]]) in
  let tup4 x1 x2 x3 x4 = Ppxlib.([%expr Caqti_type.Std.tup4 [%e x1] [%e x2] [%e x3] [%e x4]]) in
  let col_pat (col: Types.column) =
    core_ty_to_caqti_repr ~loc col.ty in
  let rec loop = function
    | [] -> assert false
    | x1 :: [] -> col_pat x1
    | x1 :: x2 :: [] ->
      tup2 (col_pat x1) (col_pat x2)
    | x1 :: x2 :: x3 :: [] ->
      tup3 (col_pat x1) (col_pat x2) (col_pat x3)
    | x1 :: x2 :: x3 :: x4 :: [] ->
      tup4 (col_pat x1) (col_pat x2) (col_pat x3) (col_pat x4)
    | x1 :: x2 :: x3 :: rest ->
      tup4 (col_pat x1) (col_pat x2) (col_pat x3) (loop rest) in
  loop table.columns

let build_encoder_from_schema ~loc name (table: Types.table) : Ppxlib.value_binding =
  let open Ppxlib in
  let pvb_pat : Astlib.Ast_412.Parsetree.pattern =
    Ast_builder.Default.(
      ppat_constraint ~loc
        (ppat_var ~loc name)
        (caqti_type ~loc name.txt)
    ) in
  let pvb_expr : Astlib.Ast_412.Parsetree.expression =
    let p_enc : Astlib.Ast_412.Parsetree.pattern =
      build_enc_pattern ~loc table in
    let p_dec : Astlib.Ast_412.Parsetree.pattern =
      build_dec_pattern ~loc table in
    let encode_expr : Astlib.Ast_412.Parsetree.expression =
      build_enc_expr ~loc table in
    let decode_expr : Astlib.Ast_412.Parsetree.expression =
      build_dec_expr ~loc table in
    let caqti_repr : Astlib.Ast_412.Parsetree.expression =
      build_caqti_repr ~loc table in
    [%expr
      let encode [%p p_enc] = Ok [%e encode_expr] in
      let decode [%p p_dec] = Ok [%e decode_expr] in
      Caqti_type.Std.custom ~encode ~decode Caqti_type.Std.([%e caqti_repr])
    ] in
  {
    pvb_pat;
    pvb_expr;
    pvb_attributes=[];
    pvb_loc=loc;
  }
