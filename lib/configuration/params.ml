[@@@warning "-34-32"]

open (struct
  type 'a param = {
    name: string;
    docv: string option;
    documentation: string;
    absent: string option;
    flags: string list option;
    mutable value: 'a;
  }

  type pref =
    | Param: 'a option param * bool * 'a Cmdliner.Arg.conv -> pref
    | Flag: bool param -> pref

  type 'a value = 'a lazy_t

  let operations = ref []

  let wrap term =
    let open Cmdliner in
    let term = ref term in
    List.iter (function
      | Flag pref ->
        let param_info =
          Arg.info
            ~doc:pref.documentation
            ~docv:(Option.value pref.docv ~default:pref.name)
            ?absent:pref.absent
            (Option.value pref.flags ~default:[pref.name]) in
        term := Term.((const (fun x term ->
          pref.value <- x;
          term
        )) $ (Arg.value (Arg.flag param_info)) $ !term)
      | Param (pref, required, param_conv) ->
        let param_info =
          Arg.info
            ~doc:pref.documentation
            ~docv:(Option.value pref.docv ~default:pref.name)
            ?absent:pref.absent
            (Option.value pref.flags ~default:[pref.name]) in
        term := if required
          then Term.((const (fun x term ->
            pref.value <- Some x;
            term
          )) $ (Arg.required (Arg.opt Arg.(some param_conv) pref.value param_info)) $ !term)
          else Term.((const (fun x term ->
            pref.value <- x;
            term
          )) $ (Arg.value (Arg.opt Arg.(some param_conv) pref.value param_info)) $ !term)
    ) !operations;
    !term


  let flag ~name ~documentation ?docv ?absent ?flags () : bool value =
    let value = {name; documentation; docv; absent; flags; value=false} in
    operations := Flag value :: !operations;
    lazy value.value

  let required ~name ~documentation ~ty ?docv ?absent ?flags () : 'a value =
    let value = {name; documentation; docv; absent; flags; value=None} in
    operations := Param (value, true, ty) :: !operations;
    lazy (match value.value with
      | None -> failwith ("parameter " ^ name ^ " was not provided")
      | Some value -> value)

  let optional ~name ~documentation ~ty ?docv ?absent ?flags () : 'a value =
    let value = {name; documentation; docv; absent; flags; value=None} in
    operations := Param (value, false, ty) :: !operations;
    lazy value.value 

  let optional_with_default ~name ~documentation ~ty ~default ?docv ?absent ?flags () : 'a value =
    let value = {name; documentation; docv; absent; flags; value=Some default} in
    operations := Param (value, false, ty) :: !operations;
    lazy (Option.get value.value)
end : sig

        type 'a value = 'a lazy_t

        val wrap : 'a Cmdliner.Term.t -> 'a Cmdliner.Term.t

        val flag :
          name:string ->
          documentation:string ->
          ?docv:string ->
          ?absent:string -> ?flags:string list -> unit -> bool value

        val required :
          name:string ->
          documentation:string ->
          ty:'a Cmdliner.Arg.conv ->
          ?docv:string -> ?absent:string -> ?flags:string list -> unit -> 'a value

        val optional :
          name:string ->
          documentation:string ->
          ty:'a Cmdliner.Arg.conv ->
          ?docv:string ->
          ?absent:string -> ?flags:string list -> unit -> 'a option value

        val optional_with_default :
          name:string ->
          documentation:string ->
          ty:'a Cmdliner.Arg.conv ->
          default:'a ->
          ?docv:string -> ?absent:string -> ?flags:string list -> unit -> 'a value

      end)

let wrap = wrap

let domain =
  optional_with_default ~name:"domain" ~docv:"DOMAIN"
    ~documentation:" $(docv) is the domain on which the server is running. This is important for verifying the signatures of incoming messages, as they will expect an appropriate domain in the header."
    ~ty:Cmdliner.Arg.string
    ~absent:{|The domain defaults to localhost.|}
    ~default:"localhost"
    ~flags:["d"; "domain"] ()

let user_image_dir =
  optional_with_default ~name:"user_image_path" ~docv:"USER-IMAGE-PATH"
    ~documentation:"$(docv) is the path to the directory used by OCamlot to store user images. The directory is created if not present."
    ~ty:Cmdliner.Arg.(dir)
    ~absent:{| A directory `user-images` in current working directory is used.|}
    ~default:"./user-images/"
    ~flags:["u"; "user-image-path"] ()

let postgres_url =
  optional ~name:"postgres_url" ~docv:"POSTGRES"
    ~documentation:"$(docv) is the url for the postgres database, if \
                    running in postgres mode."
    ~ty:Cmdliner.Arg.string
    ~absent:{|OCamlot will run using a local sqlite database.|}
    ~flags:["u"; "postgres-url"] ()

let database_path =
  optional_with_default ~name:"database_path" ~docv:"DB"
    ~documentation:"$(docv) is the path to the database used by OCamlot. The database is generated if not present."
    ~ty:Cmdliner.Arg.(file)
    ~absent:{|A fresh database is generated in the current working directory with name ocamlot.db.|}
    ~default:"./ocamlot.db"
    ~flags:["f"; "database-path"] ()

let about_this_instance =
  optional ~name:"about_this_instance" ~docv:"ABOUT-THIS-INSTANCE"
    ~documentation:"$(docv) is the path to a markdown file describing the instance. A default message is used if not provided."
    ~ty:Cmdliner.Arg.(file)
    ~absent:{|A default message is used if not provided.|}
    ~flags:["a"; "about-this-instance"] ()

let port =
  optional_with_default ~name:"port" ~docv:"PORT"
    ~documentation:"port on which the server should run."
    ~ty:Cmdliner.Arg.(int)
    ~default:7331
    ~absent:{|The port defaults to 7331.|}
    ~flags:["p"; "port"] ()

let certificate_file =
  optional ~name:"certificate_file" ~docv:"CERTIFICATE-FILE"
    ~documentation:"$(docv) is the path to the certificate file for the domain on which this server will be running."
    ~ty:Cmdliner.Arg.(file)
    ~absent:{|The server will not use TLS encryption (use a proxy like Nginx to enable ssl in that case).|}
    ~flags:["c"; "certificate-file"] ()


let key_file =
  optional ~name:"key_file" ~docv:"KEY-FILE"
    ~documentation:"key file to use for tls encryption. Both [certificate_file] and [key_file] must be provided for tls to be enabled."
    ~ty:Cmdliner.Arg.(file)
    ~absent:{|The server will not use TLS encryption (use a proxy like Nginx to enable ssl in that case).|}
    ~flags:["k"; "key-file"] ()

let debug =
  flag ~name:"debug"
    ~documentation:"Determines whether the OCamlot server should be run in debug mode."
    ~flags:["D"; "debug"] ()

let debug_dream_info =
  flag ~name:"debug-dream-info"
    ~documentation:"Determines whether the OCamlot server should run Dream with info logging."
    ~flags:["debug-dream-info"] ()

let force_migrations =
  flag ~name:"force_migrations"
    ~documentation:"Determines whether the OCamlot server should perform (potentially destructive) migrations."
    ~flags:["m"; "migrate"] ()

let dump_json_dir =
  optional ~name:"dump_json_dir" ~docv:"DUMP-JSON-DIR"
    ~documentation:"$(docv) is the path to a directory where the server should dump all JSON messages that it receives."
    ~ty:Cmdliner.Arg.(dir)
    ~absent:{|No JSON will be dumped (the default).|}
    ~flags:["dump-json-dir"] ()


let default_about_this_instance = {|
# About this instance

The site administrator for this OCamlot instance has not configured the about this page.

[OCamlot](https://codeberg/gopiandcode/ocamlot) is Free/Libre (as in FREEDOM) software that supports the FREEDOM of its users!

Donate to the FSF if you wish to support FREEDOM: [Free Software Foundation](www.fsf.org)
|}


let using_postgres = lazy (Option.is_some @@ Lazy.force postgres_url)

let is_tls_enabled =
  lazy (Option.is_some (Lazy.force certificate_file) && Option.is_some (Lazy.force key_file))
let certificate_file =
  lazy (if not (Lazy.force is_tls_enabled)
        then None
        else (Lazy.force certificate_file))

let key_file =
  lazy (if not (Lazy.force is_tls_enabled)
        then None
        else (Lazy.force key_file))

let about_this_instance = lazy begin
  Omd.of_string @@ match Lazy.force about_this_instance with
  | None -> default_about_this_instance
  | Some data ->
      let path = match Fpath.of_string data with Ok path -> path | Error (`Msg m) -> failwith m in
      let about_this_instance = match Bos.OS.File.read path with Ok data -> data | Error (`Msg m) -> failwith m in
      about_this_instance
end

let host = domain
let domain = lazy (Uri.of_string ("https://" ^ Lazy.force domain))
let database_uri =
  lazy ("sqlite3://:" ^ (Lazy.force database_path))
let port = port 
let debug = debug 
let debug_dream_info = debug_dream_info
let user_image_path =
  lazy begin
    let path =
      Result.get_ok (Fpath.of_string (Lazy.force user_image_dir)) in
    Fpath.to_string (Fpath.to_dir_path path)
  end
