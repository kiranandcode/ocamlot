type t = Types.follow

val create_follow :
  ?public_id:string ->
  ?raw_data:string ->
  ?updated:CalendarLib.Calendar.t ->
  url:string ->
  author:Actor.t Link.t ->
  target:Actor.t Link.t ->
  pending:bool ->
  created:CalendarLib.Calendar.t ->
  (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_follow_by_url : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_follow_by_url_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_follow_by_public_id :
  string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_follow_by_public_id_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val update_follow_pending_status :
  ?timestamp:CalendarLib.Calendar.t ->
  t Link.t -> bool -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val collect_follows_for_actor:
  ?offset:CalendarLib.Calendar.t * int * int ->
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val is_following: author:Actor.t Link.t -> target:Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (bool, string) Lwt_result.t

val find_follow_between: author:Actor.t Link.t -> target:Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t


val find_follow_between_exn: author:Actor.t Link.t -> target:Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val count_following:
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (int, string) Lwt_result.t

val collect_following:
  ?offset:CalendarLib.Calendar.t * int * int ->
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val count_followers:
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (int, string) Lwt_result.t

val collect_followers:
  ?offset:CalendarLib.Calendar.t * int * int ->
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t


val delete_follow:
  t Link.t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val self : t -> t Link.t
val public_id : t -> string option
val author : t -> Actor.t Link.t
val target : t -> Actor.t Link.t
val pending : t -> bool
val url : t -> string
val raw_data : t -> string option
val created: t -> CalendarLib.Calendar.t
val updated: t -> CalendarLib.Calendar.t option
