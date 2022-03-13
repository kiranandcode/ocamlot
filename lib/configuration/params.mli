type t

val create : domain:string -> database_path:string -> t

val domain: t -> Uri.t
val database_path: t -> string
