open Containers
open Types

let ident_to_ty loc ty = 
  let module B = (val Ppxlib.Ast_builder.make loc) in
  match Longident.unflatten (String.split_on_char '.' ty) with
  | Some txt ->
    Some (B.ptyp_constr Location.{txt; loc})
  | None -> None

let extract_type loc info =
  let err () = Format.ksprintf ~f:invalid_arg "invalid type specification %s" info in
  let name, ty = 
    if String.contains info ':'
    then match String.split_on_char ':' info with
      | [name; ty] ->
        let name = String.trim name in
        let ty = String.trim ty in
        Some name, ty
      | _ -> err ()
    else None, String.trim info in
  let ident_to_ty ty =
    match ident_to_ty loc ty with
    | Some ty -> ty
    | None -> err () in
  let ty = String.split_on_char ' ' ty |> List.rev in
  let ty = 
    match ty with
    | f :: args ->
      ident_to_ty f (List.map (fun v -> ident_to_ty v []) args)
    | _ -> err () in
  name, ty

let process_column loc (column: Ast.column) =
  let primary_key, is_unique, not_null =
    let primary_key = ref false in
    let is_unique = ref false in
    let not_null = ref false in
    List.iter (function
      | Ast.UNIQUE -> is_unique := true
      | PRIMARY_KEY _ -> primary_key := true
      | NOT_NULL -> not_null := true
    ) column.constraints;
    !primary_key, !is_unique, !not_null in

  match column with
  | { name; info=Some info; _} ->
    let mapped_name, ty = extract_type loc info in
    primary_key, {name; mapped_name; ty; is_unique}
  | { name; ty; info=None; _ } ->
    let ty = match ty with
      | Ast.BLOB
      | Ast.TEXT -> Option.get_exn_or "" @@ ident_to_ty loc "string"
      | Ast.INTEGER -> Option.get_exn_or "" @@ ident_to_ty loc "int64"
      | Ast.REAL -> Option.get_exn_or "" @@ ident_to_ty loc "float"
      | Ast.NUMERIC "BOOLEAN" -> Option.get_exn_or "" @@ ident_to_ty loc "bool"
      | Ast.NUMERIC _ -> Option.get_exn_or "" @@ ident_to_ty loc "int64" in
    let ty = ty [] in
    let ty =
      if not_null || primary_key
      then ty
      else (Option.get_exn_or "" @@ ident_to_ty loc "option") [ty] in
    primary_key, {name; mapped_name=None; ty; is_unique}

let process_table_constraints constraints =
  let primary_key = ref None in
  let fks = 
    List.filter_map (function
      | Ast.FOREIGN_KEY {
        key; references_table; references_key; _
      } -> Some (key, references_table, references_key)
      | PRIMARY_KEY pk ->
        primary_key := Some pk;
        None
    ) constraints in
  !primary_key, fks

let process loc program =
  List.filter_map (function
    | Ast.CREATE_TABLE {
      name; columns; constraints; info; _
    } ->
      let columns = List.map (process_column loc) columns in
      let primary_key, foreign_keys = process_table_constraints constraints in
      let primary_key = match primary_key, List.find_opt fst columns with
        | Some pk, None -> Some pk
        | None, Some (_, pk) -> Some [pk.name]
        | _ -> None in
      let columns = List.map snd columns in
      let _, ty = extract_type loc (Option.value ~default:name info) in
      Some {name; ty; columns; primary_key; foreign_keys}
    | _ -> None
  ) program
