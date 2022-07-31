module Link : sig

  type 'a t = 'a Link.t

  val resolve : 'a t -> (module Caqti_lwt.CONNECTION) -> ('a, string) Lwt_result.t

end = Link


module LocalUser = Local_user

module RemoteInstance = Remote_instance

module RemoteUser = Remote_user

module Actor = Actor


module Post = Post

module Follow = Follow

module Mention = Mention

module Like = Like

module Activity = Activity

module Tag = Tag

module Interface = Interface

