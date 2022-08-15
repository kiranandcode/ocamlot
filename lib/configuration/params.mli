type t

val create : ?port:int -> database_path:string -> string -> t

val domain: t -> Uri.t
val database_path: t -> string
val port: t -> int
