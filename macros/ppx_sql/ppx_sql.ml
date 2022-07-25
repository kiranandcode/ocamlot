[@@@warning "-27"]
open Containers
open Ppxlib
module StringSet = Set.Make (String)

let (let+) x f = x ~f

let schema_data = ref None

let setup_rule =
  Ppxlib.Context_free.Rule.special_function "declare_schema"
    (function
      | [%expr declare_schema [%e? { pexp_desc=Pexp_constant (Pconst_string (path, loc, None)); _}]]  ->
        Some (let+ contents =
                Utils.Expr.some_or_fail_expr ~loc (Utils.read_file path)
                  ~else_:"ppx_sql was unable to find a file at %s" path  in
              let+ parsed =
                Utils.Expr.ok_or_fail_expr ~loc (Sql.parse contents)
                  ~else_:"ppx_sql was unable to parse the file at %s" path in
              let+ data =
                Utils.Expr.ok_or_fail_expr ~loc (Sql.extract parsed)
                  ~else_:"ppx_sql found a unsupported SQL statement in %s" path in
              schema_data := Some data;
              ([%expr ()]))
      | {pexp_loc=loc; _} ->
        let ext =
          Utils.expression_err ~loc
            "ppx_sql's declare_schema requires a constant hardcoded string." in
        Some ext
    )

let extract_attr ~(f: string -> Ppxlib.structure) ~loc attrs =
  let schema =
      List.find_map (function
        | { attr_name={txt="schema"; _}; attr_payload; attr_loc } as attr ->
          Some (attr, attr_payload)
        | _ -> None) attrs in
  let+ schema, schema_payload =
    Utils.Structure.some_or_fail_expr ~loc schema
      ~else_:"ppx_sql sql.check requires a schema attribute on types" in
  let loc = Ppxlib.loc_of_payload schema in
  begin match schema_payload with
  | PStr [%str [%e? { pexp_desc=Pexp_constant (Pconst_string (s, loc, None)); _ }]] ->
    f s
  | _ ->
    Utils.structure_err ~loc "ppx_sql malformed schema name - expected a string with the schema name."
  end

let extract_rows ~f ~loc (decl: type_declaration) =
  match decl with
  | { ptype_params=(_ :: _); ptype_loc=loc; _ } -> 
    Utils.structure_err ~loc "ppx_sql malformed schema type - polymorphic types aren't supported."
  | { ptype_cstrs=(_ :: _); ptype_loc=loc; _ } -> 
    Utils.structure_err ~loc "ppx_sql malformed schema type - type constraints aren't supported."
  | { ptype_kind=(Ptype_abstract | Ptype_variant _ | Ptype_open); ptype_loc=loc; _ } -> 
    Utils.structure_err ~loc "ppx_sql malformed schema type - non-record types aren't supported."
  | { ptype_name={txt=label;_}; ptype_kind=Ptype_record fields; ptype_loc=loc; _ } ->
    f (loc, label, fields)

let check_declaration (columns: Sql.Types.column list) (seen_fields: StringSet.t) (label_declaration: label_declaration) ~f =
  let loc = label_declaration.pld_loc in
  let column = List.find_opt (fun (col: Sql.Types.column) ->
    String.equal (Option.value ~default:col.name col.mapped_name)
      label_declaration.pld_name.txt
  ) columns in
  let+ column = Utils.Structure.some_or_fail_expr ~loc column
                  ~else_:"field %s does not correspond to any column on the SQL table."
                  label_declaration.pld_name.txt in
  match label_declaration with
  |  { pld_type={ ptyp_desc=(Ptyp_any | Ptyp_var _ | Ptyp_arrow (_, _, _) | Ptyp_tuple _
                            | Ptyp_object (_, _) | Ptyp_class (_, _) | Ptyp_alias (_, _) | Ptyp_variant (_, _, _)
                            | Ptyp_poly (_, _) | Ptyp_package _ | Ptyp_extension _); _ }; _ } -> 
    Utils.structure_err ~loc "ppx_sql only supports primitive types or aliases."
  | {pld_type=ty;_} ->
    let ty = Format.to_string Ppxlib.Pprintast.core_type ty in
    let col_ty = Format.to_string Ppxlib.Pprintast.core_type column.ty in
    if String.equal ty col_ty
    then f (StringSet.add column.name seen_fields)
    else
      Utils.structure_err ~loc "ppx_sql expected type %s for field %s, but found type %s"
        col_ty label_declaration.pld_name.txt ty

let extract_schema_name ~f ~loc decl =
  match decl.ptype_kind with
  | Ptype_abstract |Ptype_record _ |Ptype_open | Ptype_variant ([] | _ :: _ :: _) ->
    Utils.structure_err ~loc "ppx_sql sql.generate expects a constructor [SQL [@schema <schema name>]]"
  | Ptype_variant [{ pcd_attributes=([] | _ :: _ :: _); pcd_loc=loc; _ }]
  | Ptype_variant [{ pcd_res=Some _; pcd_loc=loc; _ }]
  | Ptype_variant [{ pcd_args = Pcstr_tuple (_ :: _); pcd_loc=loc; _ }]
  | Ptype_variant [{ pcd_args = Pcstr_record (_ :: _); pcd_loc=loc; _ }] ->
    Utils.structure_err ~loc "ppx_sql sql.generate expects a constructor [SQL [@schema <schema name>]]"
  | Ptype_variant [{ pcd_name={txt=name; loc}; _ }] when not (String.equal name "SQL") ->
    Utils.structure_err ~loc "ppx_sql sql.generate expects a constructor [SQL [@schema <schema name>]]"
  | Ptype_variant [{ pcd_attributes=attributes; pcd_loc=loc; _ }] ->
    extract_attr ~f ~loc attributes



let generate_rule =
  let check
        ~(ctxt:Expansion_context.Extension.t)
        (attrs: Ppxlib__Import.attributes)
        (decl: type_declaration) : structure_item list =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let+ cached = Utils.Structure.some_or_fail_expr ~loc !schema_data
                    ~else_:"attempt to use sql.generate without specifying a SQL schema first." in
    let+ schema_name = extract_schema_name ~loc decl in
    let+ cached_table =
      Utils.Structure.some_or_fail_expr ~loc
        (List.find_opt (fun s -> String.equal s.Sql.Types.name schema_name) cached)
        ~else_:"ppx_sql was unable to find a table named %s in the \
                supplied schema. Hint: supported name %s" schema_name
        (List.map (fun s -> s.Sql.Types.name) cached |> String.concat ", ") in
    [Ast_builder.Default.pstr_type ~loc Recursive
       [(Sql.Builder.build_type_from_schema ~loc decl.ptype_name cached_table)];
     Ast_builder.Default.pstr_value ~loc Asttypes.Nonrecursive [
       Sql.Builder.build_encoder_from_schema ~loc decl.ptype_name cached_table
     ]] in
  let extension =
    Extension.V3.declare_inline "sql.generate"
      Extension.Context.structure_item
      Ast_pattern.(pstr (pstr_type recursive ((type_declaration_attributes __ __) ^:: nil) ^:: nil))
      check in
  Ppxlib.Context_free.Rule.extension extension


let check_rule =
  let check
        ~(ctxt:Expansion_context.Extension.t)
        rec_flag (attrs: Ppxlib__Import.attributes)
        (decl: type_declaration) : structure_item list =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let+ cached = Utils.Structure.some_or_fail_expr ~loc !schema_data
                    ~else_:"attempt to use sql.check without specifying a SQL schema first." in
    let+ schema_name = extract_attr ~loc attrs in
    let+ cached_table =
      Utils.Structure.some_or_fail_expr ~loc
        (List.find_opt (fun s -> String.equal s.Sql.Types.name schema_name) cached)
        ~else_:"ppx_sql was unable to find a table named %s in the \
                supplied schema. Hint: supported name %s" schema_name
        (List.map (fun s -> s.Sql.Types.name) cached |> String.concat ", ") in
    let+ loc, ty_name, fields = extract_rows ~loc decl in
    let+ seen_fields =
      Utils.foldM ~f:(check_declaration cached_table.columns) StringSet.empty fields in
    let missing_fields =
      List.filter (fun (s: Sql.Types.column) -> not @@ StringSet.mem s.name seen_fields)
        cached_table.columns
      |> function [] -> Ok () | ls ->
        Error (
          "missing fields for columns " ^
          (List.map (fun (s: Sql.Types.column) -> s.name) cached_table.columns |> String.concat ", ")
        ) in
    let+ () = Utils.Structure.ok_or_fail_expr ~loc missing_fields
                ~else_:"ppx_sql failed to validate type" in
    [Ast_builder.Default.pstr_type ~loc rec_flag [decl]] in
  let extension =
    Extension.V3.declare_inline "sql.check"
      Extension.Context.structure_item
      Ast_pattern.(pstr (pstr_type __ ((type_declaration_attributes __ __) ^:: nil) ^:: nil))
      check in
  Ppxlib.Context_free.Rule.extension extension

let query_typ_to_fun loc (ty: Sql.Query.Type.ty) query_str =
  let ty_exp = Sql.Query.Builder.enc_ty ~loc ty in
  [%expr Caqti_request.Infix.([%e ty_exp] @:- [%e query_str])]

let query_rule =

  let check ~(ctxt:Expansion_context.Extension.t) (pat: pattern) (query_str: label) (loc: location)
        (n: label option) : structure_item list =
    let+ cached = Utils.Structure.some_or_fail_expr ~loc !schema_data
                    ~else_:"attempt to use sql.query without specifying a SQL schema first." in
    let+ query =
      Utils.Structure.ok_or_fail_expr ~loc (Sql.Query.parse query_str)
        ~else_:"ppx_sql failed to parse supplied sql query" in
    let+ inferred_typ =
      Utils.Structure.ok_or_fail_expr ~loc (Sql.Query.infer cached query)
        ~else_:"ppx_sql failed to infer types for supplied sql query" in

    let res_vl = query_typ_to_fun loc inferred_typ
                   Ast_builder.Default.(pexp_constant ~loc (Pconst_string (query_str, loc, n))) in
      

    [Ast_builder.Default.pstr_value ~loc Nonrecursive [
      Ast_builder.Default.value_binding ~loc ~pat ~expr:res_vl
    ]] in
    
  let extension =
    Extension.V3.declare_inline "sql.query"
      Extension.Context.structure_item
      Ast_pattern.(pstr ((pstr_value nonrecursive
                            (value_binding
                               ~pat:__
                               ~expr:(pexp_constant (pconst_string __ __ __))
                               ^:: nil
                              )) ^:: nil))
      check in
  Ppxlib.Context_free.Rule.extension extension

let () =
  Driver.register_transformation
    ~rules:[
      setup_rule;
      generate_rule;
      check_rule;
      query_rule;
    ]
    "ppx_sql"
