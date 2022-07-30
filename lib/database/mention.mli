type t

val create_mention :
  ?public_id:string ->
  ?raw_data:string ->
  url:string ->
  author:Post.t Link.t ->
  target:Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_mention_by_url: string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_mention_by_url_exn: string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_mention_by_public_id: string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_mention_by_public_id_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val collect_mentions_for_post: Post.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val public_id : t -> string option
val url : t -> string
val raw_data : t -> string option
val post : t -> Post.t Link.t
val target : t -> Actor.t Link.t
