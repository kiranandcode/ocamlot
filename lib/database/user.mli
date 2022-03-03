
module R = Lwt_result
module type DB = Caqti_lwt.CONNECTION

type t

val create_user :
  username:string -> password:string -> (module DB) -> (t, string) R.t
val login_user :
  username:string ->
  password:string -> (module DB) -> (t option, string) R.t

val lookup_user : username:string -> (module DB) -> (t option, string) R.t
val lookup_user_exn : username:string -> (module DB) -> (t, string) R.t

val username: t -> string
val display_name: t -> string
val pubkey : t -> string
