
module Link : sig

  type 'a t = 'a Link.t

  val resolve : 'a t -> (module Caqti_lwt.CONNECTION) -> ('a, string) Lwt_result.t

end = Link


module LocalUser : sig

  type t = Local_user.t

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

  val self: t -> t Link.t
  val username: t -> string
  val display_name: t -> string
  val about: t -> string option
  val is_admin: t -> bool
  val manually_accept_follows: t -> bool
  val pubkey : t -> string
  val privkey : t -> X509.Private_key.t

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

  val inbox: t -> Uri.t
  val outbox: t -> Uri.t option
  val followers: t -> Uri.t option
  val following: t -> Uri.t option
  val summary: t -> string option
  val public_key: t -> X509.Public_key.t

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

  val collect_remote_users_following:
    ?offset:int * int -> Local_user.t Link.t ->
    (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t

end = Remote_user

module Actor : sig
  type t = Actor.t = Local of Local_user.t | Remote of Remote_user.t

  val of_local: LocalUser.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t Link.t, string) Lwt_result.t
  val of_remote: RemoteUser.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t Link.t, string) Lwt_result.t
end = Actor


module Post : sig
  type t = Post.t

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
    Post.t Link.t -> (module Caqti_lwt.CONNECTION) -> (Actor.t Link.t list, string) Lwt_result.t
  val post_cc:
    Post.t Link.t -> (module Caqti_lwt.CONNECTION) -> (Actor.t Link.t list, string) Lwt_result.t

  val add_post_to:
    Post.t Link.t -> Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
  val add_post_tos:
    Post.t Link.t -> Actor.t Link.t list -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val add_post_cc:
    Post.t Link.t -> Actor.t Link.t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
  val add_post_ccs:
    Post.t Link.t -> Actor.t Link.t list -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t


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
    Post.t Link.t ->
    Tag.t Link.t ->
    (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val add_post_tags:
    Post.t Link.t ->
    (Tag.t Link.t * string option) list ->
    (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val collect_post_tags:
    Post.t Link.t ->
    (module Caqti_lwt.CONNECTION) ->
    ((Tag.t * string option) list, string) Lwt_result.t

  val add_post_mention:
    Post.t Link.t ->
    Actor.t Link.t ->
    (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val add_post_mentions:
    Post.t Link.t ->
    Actor.t Link.t list ->
    (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

  val collect_post_mentions:
    Post.t Link.t ->
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
end = Post


module Follow : sig
  type t = Follow.t

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
end = Follow

module Mention : sig
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
end = Mention

module Like : sig
  type t = Like.t
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
end = Like

module Activity : sig
  type t = Activity.t
  type id

  val id: t -> id
  val data: t -> Yojson.Safe.t

  val url: Configuration.Params.t -> id -> Uri.t


  val id_from_string: string -> id option
  val id_to_string: id -> string

  val fresh_id : unit -> id

  val create: id:id -> data:Yojson.Safe.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val find: id -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val find_exn : id -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
  val update: t -> Yojson.Safe.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

end = Activity

module Tag : sig
  type t = Tag.t

  val create: string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  
  val find: string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
  val find_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

  val self: t -> t Link.t
  val name: t -> string
end = Tag


module Interface = Interface

