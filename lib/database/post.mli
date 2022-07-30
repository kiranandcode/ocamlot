type t = Types.post

val resolve_post: int64 -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val create_post :
  ?public_id:string ->
  ?summary:string ->
  ?raw_data:string ->
  url:string ->
  author:Actor.t Link.t ->
  is_public:bool ->
  post_source:string ->
  published:CalendarLib.Calendar.t ->
  (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val post_to:
  t Link.t -> (module Caqti_lwt.CONNECTION) -> (Actor.t Link.t list, string) Lwt_result.t
val post_cc:
  t Link.t -> (module Caqti_lwt.CONNECTION) -> (Actor.t Link.t list, string) Lwt_result.t

val add_post_to:
  t Link.t -> Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
val add_post_tos:
  t Link.t -> Actor.t Link.t list -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val add_post_cc:
  t Link.t -> Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
val add_post_ccs:
  t Link.t -> Actor.t Link.t list -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t


val lookup_post_by_url :
  string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_post_by_url_exn :
  string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val lookup_post_by_public_id :
  string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_post_by_public_id_exn :
  string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val count_posts_by_author: 
  Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (int, string) Lwt_result.t

val collect_posts_by_author:
  ?offset:CalendarLib.Calendar.t * int * int ->
  Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val add_post_tag:
  ?url:string ->
  t Link.t ->
  Tag.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val add_post_tags:
  t Link.t ->
  (Tag.t Link.t * string option) list ->
  (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val collect_post_tags:
  t Link.t ->
  (module Caqti_lwt.CONNECTION) ->
  ((Tag.t * string option) list, string) Lwt_result.t

val add_post_mention:
  t Link.t ->
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val add_post_mentions:
  t Link.t ->
  Actor.t Link.t list ->
  (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val collect_post_mentions:
  t Link.t ->
  (module Caqti_lwt.CONNECTION) ->
  (Actor.t Link.t list, string) Lwt_result.t

val collect_post_feed:
  ?offset:CalendarLib.Calendar.t * int * int ->
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val collect_post_direct:
  ?offset:CalendarLib.Calendar.t * int * int ->
  Actor.t Link.t ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val collect_post_whole_known_network:
  ?offset:CalendarLib.Calendar.t * int * int ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

val collect_post_local_network:
  ?offset:CalendarLib.Calendar.t * int * int ->
  (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t


val self : t -> t Link.t
val public_id : t -> string option
val author : t -> Actor.t Link.t
val url : t -> string
val raw_data : t -> string option
val is_public: t -> bool
val summary: t -> string option
val post_source: t -> string
val published: t -> CalendarLib.Calendar.t
