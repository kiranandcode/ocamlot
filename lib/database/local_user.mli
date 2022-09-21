type t = private Types.local_user

val resolve_user: int64 -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val create_user : ?about:string ->
  ?is_admin:bool ->
  ?manually_accepts_follows:bool ->
  username:string ->
  password:string ->
  (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val login_user :
  username:string ->
  password:string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t

val lookup_user :
  username:string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_user_exn :
  username:string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val update_about:
  t Link.t -> string -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
val update_manually_accept_follows:
  t Link.t -> bool -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
val update_is_admin:
  t Link.t -> bool -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val collect_local_users:
  ?offset:int * int -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val find_local_users:
  ?offset:int * int -> string -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val self: t -> t Link.t
val username: t -> string
val display_name: t -> string
val about: t -> string option
val is_admin: t -> bool
val manually_accept_follows: t -> bool
val pubkey : t -> string
val privkey : t -> X509.Private_key.t
