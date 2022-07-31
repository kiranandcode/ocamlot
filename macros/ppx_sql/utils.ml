open Containers
open Ppxlib

let read_file path =
  match open_in path with
  | ic ->
    Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () ->
      Some (IO.read_all ic)
    )
  | exception Not_found | exception Sys_error _ ->
    None


let expression_err ~loc =
  Format.ksprintf ~f:(fun s ->
    Ast_builder.Default.pexp_extension ~loc (Ppxlib.Location.error_extensionf ~loc "%s" s)
  )

let structure_item_err ~loc =
  let module B = (val Ast_builder.make loc) in
  Format.ksprintf ~f:(fun s ->
    B.pstr_extension (Ppxlib.Location.error_extensionf ~loc "%s" s) []
  )

let structure_err ~loc =
  let module B = (val Ast_builder.make loc) in
  Format.ksprintf ~f:(fun s ->
    [B.pstr_extension (Ppxlib.Location.error_extensionf ~loc "%s" s) []]
  )

let iterM ~f (ls: _ list) ~f:then_ =
  let (let+) x f = x ~f in
  let rec loop ls = 
    match ls with
    | [] -> then_ ()
    | h :: t ->
      let+ () = f h in
      loop t in
  loop ls

let mapM ~f ls ~f:then_ =
  let (let+) x f = x ~f in
  let rec loop acc ls = 
    match ls with
    | [] -> then_ (List.rev acc)
    | h :: t ->
      let+ h = f h in
      loop (h :: acc) t in
  loop [] ls

let foldM ~f init ls ~f:then_ =
  let (let+) x f = x ~f in
  let rec loop init ls = 
    match ls with
    | [] -> then_ init
    | h :: t ->
      let+ init = f init h in
      loop init t in
  loop init ls
        

module Expr = struct

  let ok_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Ok x -> f x
      | Error e ->
         (
          Ast_builder.Default.pexp_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s: %s" s e))
    ) else_

  let some_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Some x -> f x
      | None ->
         (
          Ast_builder.Default.pexp_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s" s))
    ) else_

end


module ExprList = struct

  let ok_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Ok x -> f x
      | Error e ->
         [(
          Ast_builder.Default.pexp_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s: %s" s e))]
    ) else_

  let some_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Some x -> f x
      | None ->
         [(
          Ast_builder.Default.pexp_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s" s))]
    ) else_

end


module Structure_item = struct

  let ok_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Ok x -> f x
      | Error e ->
         (
          Ast_builder.Default.pstr_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s: %s" s e) [])
    ) else_

  let some_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Some x -> f x
      | None ->
         (
          Ast_builder.Default.pstr_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s" s) [])
    ) else_

end

module Structure = struct

  let ok_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Ok x -> f x
      | Error e ->
         (
          [Ast_builder.Default.pstr_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s: %s" s e) []])
    ) else_

  let some_or_fail_expr ~f ~loc x ~else_ =
    Format.ksprintf ~f:(fun s -> 
      match x with
      | Some x -> f x
      | None ->
         (
          [Ast_builder.Default.pstr_extension ~loc
            (Ppxlib.Location.error_extensionf ~loc "%s" s) []])
    ) else_

end

