type t = Types.remote_user

val self : t -> t Link.t
val username : t -> string
val instance : t -> Remote_instance.t Link.t
val display_name : t -> string
val url : t -> string

val inbox: t -> Uri.t
val outbox: t -> Uri.t option
val followers: t -> Uri.t option
val following: t -> Uri.t option
val summary: t -> string option
val public_key: t -> X509.Public_key.t

val resolve_remote_user : int64 -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val create_remote_user:
  ?display_name:string -> ?inbox:string -> ?outbox:string ->
  ?followers:string -> ?following:string -> ?summary:string ->
  username:string ->
  instance:Remote_instance.t Link.t ->
  url:string -> public_key_pem:string ->
  (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_remote_user_by_url: string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_remote_user_by_url_exn: string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val lookup_remote_user_by_address: username:string -> domain:string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_remote_user_by_address_exn : username:string -> domain:string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val get_known_remote_actors : (module Caqti_lwt.CONNECTION) -> ((string * string * string) list, string) Lwt_result.t

val find_remote_users: ?offset:int * int -> string -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val collect_remote_users_following:
  ?offset:int * int -> Local_user.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t


