open Containers

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

let int = Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {txt=Longident.Lident "int"; loc=Location.none} []
let bool = Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {txt=Longident.Lident "bool"; loc=Location.none} []
let ty n args = Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {txt=Longident.Lident n; loc=Location.none} args

let datetime =
  Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {
    txt=Longident.Ldot (Lident "Calendar", "t");
    loc=Location.none
  } []

let lookup all_tables table_map table_context ?table_name column =
  match table_name with
  | None ->
    List.find_map (fun (table: Types.table) ->
      List.find_map (fun (col: Types.column) ->
        if String.equal col.name column
        then Some (table, col)
        else None
      ) table.columns
    ) table_context
    |> Option.get_exn_or (Format.sprintf "found use of column %s not present in context" column)
  | Some table_name ->
    let table_name =
      match StringMap.find_opt table_name table_map with
      | Some v -> v
      | None -> table_name in
    let table =
      List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table_name) all_tables
      |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table_name) in
    List.find_map (fun (col: Types.column) ->
      if String.equal col.name column
      then Some (table, col)
      else None
    ) table.columns
    |> Option.get_exn_or (Format.sprintf "found use of column %s not present in the table %s" column table_name)


let type_of_sql_query_value all_tables (table_map: string StringMap.t) (tables: Types.table list) (value: Query_ast.sql_value) :
  Query_type.core_type =
  match value with
  | Query_ast.C (table_name, column) ->
    (snd (lookup all_tables table_map tables ?table_name column)).ty
  | Query_ast.COUNT _ -> int
  | Query_ast.STAR -> failwith "STAR const not expected in this context"
  | Query_ast.INT _ -> failwith "INT const not expected in this context"
  | Query_ast.BOOL _ -> failwith "BOOL const not expected in this context"
  | Query_ast.COALESCE _ -> failwith "COALESCE not expected in this context"
  | Query_ast.DATETIME _ -> failwith "DATETIME not expected in this context"
  | Query_ast.HOLE _ -> failwith "hole not expected in this context"

let rec type_of_expected_constant_sql_value all_tables
          (table_map: string StringMap.t)
          (tables: Types.table list)
          (value: Query_ast.sql_value) : Query_type.core_type option =
  match value with
  | Query_ast.C (table_name, column) ->
    Some (snd (lookup all_tables table_map tables ?table_name column)).ty
  | Query_ast.COUNT _ -> Some int
  | Query_ast.STAR -> None
  | Query_ast.INT _ -> Some int
  | Query_ast.BOOL _ -> Some bool
  | Query_ast.COALESCE vls -> List.find_map (type_of_expected_constant_sql_value all_tables table_map tables) vls
  | Query_ast.DATETIME _ -> Some datetime
  | Query_ast.HOLE _ -> None

let rec assert_no_holes (value: Query_ast.sql_value) =
  match value with
  | Query_ast.C (_, _) -> ()
  | Query_ast.COUNT vl -> assert_no_holes vl
  | Query_ast.STAR -> ()
  | Query_ast.INT _ -> ()
  | Query_ast.BOOL _ -> ()
  | Query_ast.COALESCE vls ->
    List.iter assert_no_holes vls
  | Query_ast.DATETIME vl -> assert_no_holes vl
  | Query_ast.HOLE _ -> failwith "hole not expected in this context"

let rec visit_sql_value (value: Query_ast.sql_value) ty mapping =
  match value with
  | Query_ast.C (_, _) -> mapping
  | Query_ast.COUNT _ -> mapping
  | Query_ast.STAR -> mapping
  | Query_ast.INT _ -> mapping
  | Query_ast.BOOL _ -> mapping
  | Query_ast.COALESCE vls ->
    List.fold_left (fun mapping value -> visit_sql_value value ty mapping) mapping vls
  | Query_ast.DATETIME dt -> assert_no_holes dt; mapping
  | Query_ast.HOLE n ->
    IntMap.add n ty mapping

let rec check_where_constraint_unique all_tables (table_map: string StringMap.t) (tables: Types.table list)
          (where: Query_ast.where_constraint) =
  match where with
  | Query_ast.TRUE -> false
  | Query_ast.FALSE -> false
  | Query_ast.EQ ((table_name, column), _) ->
    let table, col = lookup all_tables table_map tables ?table_name column in
    let is_primary = match table.primary_key with
      | Some [key] -> String.equal key column
      | _ -> false in
    let is_fk = List.exists (fun (col, _, _) -> String.equal col column) table.foreign_keys in
    col.is_unique || is_primary || is_fk
  | Query_ast.AND (l, r) ->
    check_where_constraint_unique all_tables table_map tables l ||
    check_where_constraint_unique all_tables table_map tables r
  | Query_ast.OR (l, r) ->
    check_where_constraint_unique all_tables table_map tables l && check_where_constraint_unique all_tables table_map tables r
  | Query_ast.LEQ (_, _) 
  | Query_ast.LT (_, _)
  | Query_ast.GEQ (_, _)
  | Query_ast.GT (_, _) -> false
  | Query_ast.IS_NOT_NULL _ -> false
  | Query_ast.EXISTS _ -> false


let rec visit_where_constraint all_tables (table_map: string StringMap.t) (tables: Types.table list)
          (where: Query_ast.where_constraint) mapping =
  match where with
  | Query_ast.TRUE -> mapping
  | Query_ast.FALSE -> mapping
  | Query_ast.EQ (known, unknown) ->
    let ty = type_of_expected_constant_sql_value  all_tables table_map tables (Query_ast.C known)
             |> Option.get_exn_or "expected a constant type on LHS of equality" in
    visit_sql_value unknown ty mapping
  | Query_ast.AND (l, r)
  | Query_ast.OR (l, r) ->
    visit_where_constraint all_tables table_map tables l mapping
    |> visit_where_constraint all_tables table_map tables r
  | Query_ast.LEQ (l, r) 
  | Query_ast.LT (l, r)
  | Query_ast.GEQ (l, r)
  | Query_ast.GT (l, r) ->
    begin match
      (type_of_expected_constant_sql_value all_tables table_map tables l),
      (type_of_expected_constant_sql_value all_tables table_map tables r) with
    | Some ty, Some ty' ->
      visit_sql_value l ty mapping
      |> visit_sql_value r ty'
    | Some ty, None ->
      visit_sql_value r ty mapping
    | None, Some ty ->
      visit_sql_value l ty mapping
    | None, None -> assert_no_holes l; assert_no_holes r; mapping
    end
  | Query_ast.IS_NOT_NULL vl ->
    begin match type_of_expected_constant_sql_value all_tables table_map tables vl with
    | Some ty -> visit_sql_value vl ty mapping
    | None -> mapping
    end
  | Query_ast.EXISTS select_query ->
    visit_select_query all_tables table_map tables select_query mapping
and visit_select_query all_tables (table_map: string StringMap.t) (tables: Types.table list)
      {
        table; table_name;
        join_constraint;
        where_constraint;
        order_by;
        limit; offset;
        _
      } mapping =
  let table_map = match table_name with
      None -> table_map
    | Some table_name -> StringMap.add table_name table table_map in
  let tables = 
    List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table) all_tables
    |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
    |> Fun.flip List.cons tables in
  let table_map, tables =
    match join_constraint with
    | None -> table_map, tables
    | Some { table; table_name; _ } ->
      let table_map = match table_name with
          None -> table_map
        | Some table_name -> StringMap.add table_name table table_map in
      let tables = 
        List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table) all_tables
        |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
        |> Fun.flip List.cons tables in
      table_map, tables in
  let mapping = match where_constraint with
    | None -> mapping
    | Some where_constraint ->
      visit_where_constraint all_tables table_map tables where_constraint mapping in
  let mapping = match order_by with
    | None -> mapping
    | Some value -> assert_no_holes value; mapping in
  let mapping = match limit with
    | None -> mapping
    | Some value ->
      visit_sql_value value int mapping in
  let mapping = match offset with
    | None -> mapping
    | Some value ->
      visit_sql_value value int mapping in
  mapping

let combine_opt ls rs =
  let rec loop acc ls rs =
    match ls, rs with
    | [], (_ :: _)
    | (_ :: _), [] -> None
    | [], [] -> Some (List.rev acc)
    | l :: ls, r :: rs ->
      loop ((l,r) :: acc) ls rs in
  loop [] ls rs

let simplify_types (tables: Types.table list) (tys: Query_type.core_type list) : Query_type.core_type list =
  let check_eq (col: Types.column) (cty: Parsetree.core_type) =
    let to_string s = Format.to_string Ppxlib.Pprintast.core_type s in
    String.equal (to_string col.ty) (to_string cty) in
  List.find_map (fun (table: Types.table) ->
    match combine_opt table.columns tys with
    | None -> None
    | Some combined ->
      if List.for_all (Fun.uncurry check_eq) combined
      then Some ([table.ty])
      else None
  ) tables
  |> Option.value ~default:tys
     

let type_of_query (tables: Types.table list) (query: Query_ast.query) : Query_type.ty =
  (match query with
   | Query_ast.SELECT {
     values;
     table;
     table_name;
     join_constraint;
     where_constraint;
     order_by;
     limit;
     offset; _
   } ->
     let table_map = StringMap.empty in
     let table_map = match table_name with
         None -> table_map
       | Some table_name -> StringMap.add table_name table table_map in
     let table_context = 
       List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table) tables
       |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
       |> Fun.flip List.cons [] in
     let table_map, table_context =
       match join_constraint with
       | None -> table_map, table_context
       | Some { table; table_name; _ } ->
         let table_map = match table_name with
             None -> table_map
           | Some table_name -> StringMap.add table_name table table_map in
         let table_context = 
           List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table) tables
           |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
           |> Fun.flip List.cons table_context in
         table_map, table_context in
     let returning_multiple =
       match where_constraint with
       | None -> true
       | Some where_constraint ->
         not @@ check_where_constraint_unique tables table_map table_context where_constraint in
     let ret_ty : Query_type.ret_ty = 
       match values with
       | [STAR] ->
         let tys = 
           List.concat_map (fun (tbl: Types.table) ->
             List.map (fun (col: Types.column) -> col.ty) tbl.columns
           )  table_context in
         let tys = simplify_types table_context tys in
         Tuple {many=returning_multiple; tys}
       | tys ->
         let tys = List.map (type_of_sql_query_value tables table_map table_context) tys in
         let tys = simplify_types table_context tys in
         Tuple {many=returning_multiple; tys} in
     let hole_types = IntMap.empty in
     let hole_types =
       match where_constraint with
       | None -> hole_types
       | Some where_constraint ->
         visit_where_constraint tables table_map table_context
           where_constraint hole_types in
     let hole_types = match order_by with
       | None -> hole_types
       | Some value -> assert_no_holes value; hole_types in
     let hole_types = match limit with
       | None -> hole_types
       | Some value ->
         visit_sql_value value int hole_types in
     let hole_types = match offset with
       | None -> hole_types
       | Some value ->
         visit_sql_value value int hole_types in
     let arg_types : Query_type.arg_ty =
       match IntMap.max_binding_opt hole_types with
       | None -> Unit
       | Some (mb, _) ->
         let tys = (List.init (mb + 1) (Fun.flip IntMap.find hole_types)) in
         let tys = simplify_types table_context tys in
         Tuple tys in
     Arrow (arg_types, ret_ty)
   | Query_ast.INSERT { table=table; columns; _ } ->
     let table =
       List.find (fun (tbl: Types.table) -> String.equal tbl.name table)
         tables in
     let ret_ty : Query_type.ret_ty = Unit in
     let arg_ty : Query_type.arg_ty =
       let tys =
         List.map (fun col ->
           let col = 
             List.find (fun (c : Types.column) -> String.equal c.name col)
               table.columns in
           col.ty
         ) columns in
       let tys = simplify_types tables tys in
       Tuple tys in
     Arrow (arg_ty, ret_ty)
   | Query_ast.UPDATE { table; set; where_constraint; _ } ->
     let table_map = StringMap.empty in
     let table_context = 
       List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table) tables
       |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
       |> Fun.flip List.cons [] in
     let ret_ty : Query_type.ret_ty = Unit in
     let hole_types = IntMap.empty in
     let hole_types =
       List.fold_left (fun hole_types ((table_name, col), value) ->
         let ty = (snd (lookup tables table_map table_context ?table_name col)).ty in
         visit_sql_value value ty hole_types
       ) hole_types set in
     let hole_types =
       visit_where_constraint tables table_map table_context
         where_constraint hole_types in
     let arg_types : Query_type.arg_ty =
       match IntMap.max_binding_opt hole_types with
       | None -> Unit
       | Some (mb, _) ->
         let tys = (List.init (mb + 1) (Fun.flip IntMap.find hole_types)) in
         let tys = simplify_types table_context tys in
         Tuple tys in
     Arrow (arg_types, ret_ty)
   | Query_ast.DELETE { table; where_constraint; limit } ->
     let table_map = StringMap.empty in
     let table_context = 
       List.find_opt (fun (tbl: Types.table) -> String.equal tbl.name table) tables
       |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
       |> Fun.flip List.cons [] in
     let ret_ty : Query_type.ret_ty = Unit in
     let hole_types = IntMap.empty in
     let hole_types =
       visit_where_constraint tables table_map table_context
         where_constraint hole_types in
     let hole_types = match limit with
       | None -> hole_types
       | Some value -> visit_sql_value value int hole_types in
     let arg_types : Query_type.arg_ty =
       match IntMap.max_binding_opt hole_types with
       | None -> Unit
       | Some (mb, _) ->
         let tys = (List.init (mb + 1) (Fun.flip IntMap.find hole_types)) in
         let tys = simplify_types table_context tys in
         Tuple tys in
     Arrow (arg_types, ret_ty)
  )
