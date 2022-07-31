[@@@warning "-27"]
open Ppxlib

module Activitypub_encode = struct

  let name = "ap.encode"

  let str_type_decl = assert false
  let sig_type_decl = assert false
  let extension = assert false

  let deriver =
    Deriving.add name
      ~str_type_decl
      ~sig_type_decl
      ~extension

  let () =
    Driver.register_transformation name
      ~rules:[
        Context_free.Rule.extension (
          Extension.declare name Core_type
            Ast_pattern.(ptyp __)
            (fun ~loc ~path ty -> ty)
        )
      ]
  
end

module Activitypub_decode = struct

  let name = "ap.decode"

  let str_type_decl = assert false
  let sig_type_decl = assert false
  let extension = assert false

  let deriver =
    Deriving.add name
      ~str_type_decl
      ~sig_type_decl
      ~extension

  let () =
    Driver.register_transformation name
      ~rules:[
        Context_free.Rule.extension (
          Extension.declare name Core_type
            Ast_pattern.(ptyp __)
            (fun ~loc ~path ty -> ty)
        )
      ]
  
end

let sig_type_decl = assert false
let deriver =
  Deriving.add "ppx_activitypub: string that would'nt parse if put in the source"
    ~sig_type_decl


let activitypub =
  Deriving.add_alias "activitypub"
    [ Activitypub_encode.deriver; Activitypub_decode.deriver ]
    ~sig_type_decl:[ deriver ]
