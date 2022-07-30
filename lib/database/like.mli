type t = Types.like
val create :
  ?public_id:string ->
  ?raw_data:string ->
  url:string ->
  post:Post.t Link.t ->
  actor:Actor.t Link.t ->
  published:CalendarLib.Calendar.t ->
  (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_like_by_url : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_like_by_url_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_like_by_public_id :
  string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_like_by_public_id_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val collect_likes_for_post :
  Post.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val collect_likes_for_actor :
  ?offset:(CalendarLib.Calendar.t * int * int) ->
  Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t


val self : t -> t Link.t
val public_id : t -> string option
val url : t -> string
val raw_data : t -> string option
val post : t -> Post.t Link.t
val target : t -> Actor.t Link.t
val published: t -> CalendarLib.Calendar.t
