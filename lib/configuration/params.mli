type t

val create :
  ?key_file:string -> ?certificate_file:string ->
  ?about_this_instance:string -> ?debug:bool -> ?port:int -> database_path:string -> string -> t

val is_tls_enabled: t -> bool
val certificate_file: t -> string option
val key_file: t -> string option
val about_this_instance: t -> Omd.doc
val domain: t -> Uri.t
val database_path: t -> string
val port: t -> int
val debug: t -> bool
