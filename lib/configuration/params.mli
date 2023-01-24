type 'a t

val create :
  ?key_file:string -> ?certificate_file:string ->
  ?about_this_instance:string -> ?debug:bool -> ?port:int -> user_image_path:string -> database_path:string -> string -> 'a t


val send_task: 'a t -> 'a -> unit
val set_task_fn: 'a t -> ('a option -> unit) -> unit

val is_tls_enabled: 'a t -> bool
val certificate_file: 'a t -> string option
val key_file: 'a t -> string option
val about_this_instance: 'a t -> Omd.doc
val host: 'a t -> string
val domain: 'a t -> Uri.t
val database_path: 'a t -> string
val port: 'a t -> int
val debug: 'a t -> bool
val user_image_path: 'a t -> string
