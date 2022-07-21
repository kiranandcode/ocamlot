open Ppxlib

let expression_err ~loc =
  Format.ksprintf (fun s ->
    Ast_builder.Default.pexp_extension ~loc (Ppxlib.Location.error_extensionf ~loc "%s" s)
  )

let schema_path = ref None

let read_file path =
  match open_in path with
  | ic ->
    Fun.protect ~finally:(fun () -> In_channel.close_noerr ic) (fun () ->
      Some (In_channel.input_all ic)
    )
  | exception Not_found | exception Sys_error _ ->
    None


let setup_rule =
  Ppxlib.Context_free.Rule.special_function "declare_schema"
    (function
      | [%expr declare_schema [%e? { pexp_desc=Pexp_constant (Pconst_string (path, loc, None)); _}]]  ->
        begin match read_file path with
        | Some contents ->
          schema_path := Some contents;
          Some ([%expr ()])
        | None ->
          let err =
            expression_err ~loc
              "ppx_sql was unable to find a file at %s" path in
          Some err
        end
      | {pexp_loc=loc; _} ->
        let ext = expression_err ~loc "ppx_sql's declare_schema requires a constant hardcoded string." in
        Some ext
    )

let load_rule =
  let load_schema ~ctxt:(ctxt: Expansion_context.Extension.t) _ =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let module B = (val Ast_builder.make loc) in
    match !schema_path with
    | None ->
      B.pexp_constant (Pconst_string ("not found", loc, None))
    | Some path ->
      B.pexp_constant (Pconst_string (path, loc, None))
  in
  let extension =
    Extension.V3.declare "sql"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (estring __)) load_schema in

  Ppxlib.Context_free.Rule.extension
    extension

let () =
  Driver.register_transformation
    ~rules:[
      setup_rule;
      load_rule
    ]
    "ppx_sql"
