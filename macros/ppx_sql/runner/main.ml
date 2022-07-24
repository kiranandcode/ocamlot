open Containers

let file = "../../../resources/schema.sql"
let schema = IO.with_in file IO.read_all |> Sql.parse |> Result.get_exn |> Sql.extract |> Result.get_exn

let file = "../../test/example_queries.sql"

let queries = IO.with_in file IO.read_all
              |> String.split ~by:";;\n"


module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

let int = Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {txt=Longident.Lident "int"; loc=Location.none} []
let bool = Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {txt=Longident.Lident "bool"; loc=Location.none} []
let datetime =
  Ppxlib.Ast_builder.Default.ptyp_constr ~loc:Location.none {
    txt=Longident.Ldot (Lident "Calendar", "t");
    loc=Location.none
  } []

let lookup all_tables table_map table_context ?table_name column =
  match table_name with
  | None ->
    List.find_map (fun (table: Sql.Types.table) ->
      List.find_map (fun (col: Sql.Types.column) ->
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
      List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table_name) all_tables
      |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table_name) in
    List.find_map (fun (col: Sql.Types.column) ->
      if String.equal col.name column
      then Some (table, col)
      else None
    ) table.columns
    |> Option.get_exn_or (Format.sprintf "found use of column %s not present in the table %s" column table_name)


let type_of_sql_query_value all_tables (table_map: string StringMap.t) (tables: Sql.Types.table list) (value: Sql.Query.Ast.sql_value) :
  Sql.Query.Type.core_type =
  match value with
  | Sql.Query.Ast.C (table_name, column) ->
    (snd (lookup all_tables table_map tables ?table_name column)).ty
  | Sql.Query.Ast.COUNT _ -> int
  | Sql.Query.Ast.STAR -> failwith "STAR const not expected in this context"
  | Sql.Query.Ast.INT _ -> failwith "INT const not expected in this context"
  | Sql.Query.Ast.BOOL _ -> failwith "BOOL const not expected in this context"
  | Sql.Query.Ast.COALESCE _ -> failwith "COALESCE not expected in this context"
  | Sql.Query.Ast.DATETIME _ -> failwith "DATETIME not expected in this context"
  | Sql.Query.Ast.HOLE _ -> failwith "hole not expected in this context"

let rec type_of_expected_constant_sql_value all_tables
          (table_map: string StringMap.t)
          (tables: Sql.Types.table list)
          (value: Sql.Query.Ast.sql_value) : Sql.Query.Type.core_type option =
  match value with
  | Sql.Query.Ast.C (table_name, column) ->
    Some (snd (lookup all_tables table_map tables ?table_name column)).ty
  | Sql.Query.Ast.COUNT _ -> Some int
  | Sql.Query.Ast.STAR -> None
  | Sql.Query.Ast.INT _ -> Some int
  | Sql.Query.Ast.BOOL _ -> Some bool
  | Sql.Query.Ast.COALESCE vls -> List.find_map (type_of_expected_constant_sql_value all_tables table_map tables) vls
  | Sql.Query.Ast.DATETIME _ -> Some datetime
  | Sql.Query.Ast.HOLE _ -> None

let rec assert_no_holes (value: Sql.Query.Ast.sql_value) =
  match value with
  | Sql.Query.Ast.C (_, _) -> ()
  | Sql.Query.Ast.COUNT vl -> assert_no_holes vl
  | Sql.Query.Ast.STAR -> ()
  | Sql.Query.Ast.INT _ -> ()
  | Sql.Query.Ast.BOOL _ -> ()
  | Sql.Query.Ast.COALESCE vls ->
    List.iter assert_no_holes vls
  | Sql.Query.Ast.DATETIME vl -> assert_no_holes vl
  | Sql.Query.Ast.HOLE _ -> failwith "hole not expected in this context"

let rec visit_sql_value (value: Sql.Query.Ast.sql_value) ty mapping =
  match value with
  | Sql.Query.Ast.C (_, _) -> mapping
  | Sql.Query.Ast.COUNT _ -> mapping
  | Sql.Query.Ast.STAR -> mapping
  | Sql.Query.Ast.INT _ -> mapping
  | Sql.Query.Ast.BOOL _ -> mapping
  | Sql.Query.Ast.COALESCE vls ->
    List.fold_left (fun mapping value -> visit_sql_value value ty mapping) mapping vls
  | Sql.Query.Ast.DATETIME dt -> assert_no_holes dt; mapping
  | Sql.Query.Ast.HOLE n ->
    IntMap.add n ty mapping

let rec check_where_constraint_unique all_tables (table_map: string StringMap.t) (tables: Sql.Types.table list)
          (where: Sql.Query.Ast.where_constraint) =
  match where with
  | Sql.Query.Ast.TRUE -> false
  | Sql.Query.Ast.FALSE -> false
  | Sql.Query.Ast.EQ ((table_name, column), _) ->
    let table, col = lookup all_tables table_map tables ?table_name column in
    let is_primary = match table.primary_key with
      | Some [key] -> String.equal key column
      | _ -> false in
    col.is_unique || is_primary
  | Sql.Query.Ast.AND (l, r) ->
    check_where_constraint_unique all_tables table_map tables l ||
    check_where_constraint_unique all_tables table_map tables r
  | Sql.Query.Ast.OR (l, r) ->
    check_where_constraint_unique all_tables table_map tables l && check_where_constraint_unique all_tables table_map tables r
  | Sql.Query.Ast.LEQ (_, _) 
  | Sql.Query.Ast.LT (_, _)
  | Sql.Query.Ast.GEQ (_, _)
  | Sql.Query.Ast.GT (_, _) -> false
  | Sql.Query.Ast.IS_NOT_NULL _ -> false
  | Sql.Query.Ast.EXISTS _ -> false


let rec visit_where_constraint all_tables (table_map: string StringMap.t) (tables: Sql.Types.table list)
          (where: Sql.Query.Ast.where_constraint) mapping =
  match where with
  | Sql.Query.Ast.TRUE -> mapping
  | Sql.Query.Ast.FALSE -> mapping
  | Sql.Query.Ast.EQ (known, unknown) ->
    let ty = type_of_expected_constant_sql_value  all_tables table_map tables (Sql.Query.Ast.C known)
             |> Option.get_exn_or "expected a constant type on LHS of equality" in
    visit_sql_value unknown ty mapping
  | Sql.Query.Ast.AND (l, r)
  | Sql.Query.Ast.OR (l, r) ->
    visit_where_constraint all_tables table_map tables l mapping
    |> visit_where_constraint all_tables table_map tables r
  | Sql.Query.Ast.LEQ (l, r) 
  | Sql.Query.Ast.LT (l, r)
  | Sql.Query.Ast.GEQ (l, r)
  | Sql.Query.Ast.GT (l, r) ->
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
  | Sql.Query.Ast.IS_NOT_NULL vl ->
    begin match type_of_expected_constant_sql_value all_tables table_map tables vl with
    | Some ty -> visit_sql_value vl ty mapping
    | None -> mapping
    end
  | Sql.Query.Ast.EXISTS select_query ->
    visit_select_query all_tables table_map tables select_query mapping
and visit_select_query all_tables (table_map: string StringMap.t) (tables: Sql.Types.table list)
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
    List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table) all_tables
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
        List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table) all_tables
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

let type_of_query (tables: Sql.Types.table list) (query: Sql.Query.Ast.query) : Sql.Query.Type.ty =
  (match query with
   | Sql.Query.Ast.SELECT {
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
       List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table) tables
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
           List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table) tables
           |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
           |> Fun.flip List.cons table_context in
         table_map, table_context in
     let returning_multiple =
       match where_constraint with
       | None -> true
       | Some where_constraint ->
         check_where_constraint_unique tables table_map table_context where_constraint in
     let ret_ty : Sql.Query.Type.ret_ty = 
       match values with
       | [STAR] ->
         let tys = 
           List.concat_map (fun (tbl: Sql.Types.table) ->
             List.map (fun (col: Sql.Types.column) -> col.ty) tbl.columns
           )  table_context in
         Tuple {many=returning_multiple; tys}
       | tys ->
         let tys = List.map (type_of_sql_query_value tables table_map table_context) tys in
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
     let arg_types : Sql.Query.Type.arg_ty =
       match IntMap.max_binding_opt hole_types with
       | None -> Unit
       | Some (mb, _) ->
         Tuple (List.init (mb + 1) (Fun.flip IntMap.find hole_types)) in
     Arrow (arg_types, ret_ty)
   | Sql.Query.Ast.INSERT { table=table; columns; _ } ->
     let table =
       List.find (fun (tbl: Sql.Types.table) -> String.equal tbl.name table)
         tables in
     let ret_ty : Sql.Query.Type.ret_ty = Unit in
     let arg_ty : Sql.Query.Type.arg_ty =
       let tys =
         List.map (fun col ->
           let col = 
             List.find (fun (c : Sql.Types.column) -> String.equal c.name col)
               table.columns in
           col.ty
         ) columns in
       Tuple tys in
     Arrow (arg_ty, ret_ty)
   | Sql.Query.Ast.UPDATE { table; set; where_constraint; _ } ->
     let table_map = StringMap.empty in
     let table_context = 
       List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table) tables
       |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
       |> Fun.flip List.cons [] in
     let ret_ty : Sql.Query.Type.ret_ty = Unit in
     let hole_types = IntMap.empty in
     let hole_types =
       List.fold_left (fun hole_types ((table_name, col), value) ->
         let ty = (snd (lookup tables table_map table_context ?table_name col)).ty in
         visit_sql_value value ty hole_types
       ) hole_types set in
     let hole_types =
       visit_where_constraint tables table_map table_context
         where_constraint hole_types in
     let arg_types : Sql.Query.Type.arg_ty =
       match IntMap.max_binding_opt hole_types with
       | None -> Unit
       | Some (mb, _) ->
         Tuple (List.init (mb + 1) (Fun.flip IntMap.find hole_types)) in
     Arrow (arg_types, ret_ty)
   | Sql.Query.Ast.DELETE { table; where_constraint; limit } ->
     let table_map = StringMap.empty in
     let table_context = 
       List.find_opt (fun (tbl: Sql.Types.table) -> String.equal tbl.name table) tables
       |> Option.get_exn_or (Format.sprintf "found use of table %s not present in the schema" table)
       |> Fun.flip List.cons [] in
     let ret_ty : Sql.Query.Type.ret_ty = Unit in
     let hole_types = IntMap.empty in
     let hole_types =
       visit_where_constraint tables table_map table_context
         where_constraint hole_types in
     let hole_types = match limit with
       | None -> hole_types
       | Some value -> visit_sql_value value int hole_types in
     let arg_types : Sql.Query.Type.arg_ty =
       match IntMap.max_binding_opt hole_types with
       | None -> Unit
       | Some (mb, _) ->
         Tuple (List.init (mb + 1) (Fun.flip IntMap.find hole_types)) in
     Arrow (arg_types, ret_ty)
  )

let () =
  List.iteri (fun i s ->
    match Sql.Query.parse s with
    | Ok query ->
      begin match type_of_query schema query with
      | _ty -> ()
        (* Format.printf "[%d]: %a\n%s@." i Sql.Query.Type.pp_ty ty s *)
      | exception e ->
        Format.printf "[%d]: exn %s\n" i  (Printexc.to_string e);
        Printexc.print_backtrace stdout;
        Format.printf "[%d]: %s\n" i s;
      end
    | Error e ->
      Format.printf "[%d]: failed to parse because %s\n%s" i e s
    | exception e ->
      Format.printf "[%d]: exn %s@." i  (Printexc.to_string e);
      Printexc.print_backtrace stdout;
  ) queries
