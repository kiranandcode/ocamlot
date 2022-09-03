type t

val create :  ?about_this_instance:string -> ?debug:bool -> ?port:int -> database_path:string -> string -> t

val about_this_instance: t -> Omd.doc
val domain: t -> Uri.t
val database_path: t -> string
val port: t -> int
val debug: t -> bool
