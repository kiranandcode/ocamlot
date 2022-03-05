
module Link : sig

  type 'a t = 'a Link.t

  val resolve : 'a t -> (module Caqti_lwt.CONNECTION) -> ('a, string) Lwt_result.t

end = Link


module LocalUser : sig

  type t = Local_user.t

  val create_user :
    username:string -> password:string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val login_user :
    username:string ->
    password:string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t

  val lookup_user : username:string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_user_exn : username:string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val username: t -> string
  val display_name: t -> string
  val pubkey : t -> string

end = Local_user

module RemoteInstance : sig
  type t = Remote_instance.t

  val create_instance : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val lookup_instance : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_instance_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val record_instance_reachable: t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
  val record_instance_unreachable: t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val self : t -> t Link.t
  val url : t -> string
  val last_unreachable : t -> CalendarLib.Calendar.t option
end = Remote_instance

module RemoteUser : sig
  type t = Remote_user.t

  val self : t -> t Link.t
  val username : t -> string
  val instance : t -> Remote_instance.t Link.t
  val display_name : t -> string
  val url : t -> string

  val create_remote_user:
    ?display_name:string ->
    username:string ->
    instance:Remote_instance.t Link.t ->
    url:string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val lookup_remote_user_by_url: string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_remote_user_by_url_exn: string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val lookup_remote_user_by_address: username:string -> domain:string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_remote_user_by_address_exn : username:string -> domain:string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val get_known_remote_actors : (module Caqti_lwt.CONNECTION) -> ((string * string * string) list, string) Lwt_result.t
end = Remote_user

module Actor : sig
  type t = Actor.t = Local of Local_user.t | Remote of Remote_user.t

  val of_local: Local_user.t -> (module Caqti_lwt.CONNECTION) -> (t Link.t, string) Lwt_result.t
  val of_remote: Remote_user.t -> (module Caqti_lwt.CONNECTION) -> (t Link.t, string) Lwt_result.t
end = Actor


module Post : sig
  type t = Post.t

  val create_post :
    ?public_id:string ->
    ?raw_data:string ->
    url:string ->
    author:Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val lookup_post_by_url : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_post_by_url_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val lookup_post_by_public_id : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_post_by_public_id_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val collect_posts_by_author: Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

  val self : t -> t Link.t
  val public_id : t -> string option
  val author : t -> Actor.t Link.t
  val url : t -> string
  val raw_data : t -> string option
end = Post


module Follow : sig
  type t = Follow.t

  val create_follow :
    ?public_id:string ->
    ?raw_data:string ->
    ?pending:bool ->
    url:string ->
    author:Actor.t Link.t ->
    target:Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val lookup_follow_by_url : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_follow_by_url_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val lookup_follow_by_public_id :
    string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val lookup_post_by_public_id_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val update_follow_pending_status :
    t -> bool option -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val self : t -> t Link.t
  val public_id : t -> string option
  val author : t -> Actor.t Link.t
  val target : t -> Actor.t Link.t
  val pending : t -> bool option
  val url : t -> string
  val raw_data : t -> string option
end = Follow
