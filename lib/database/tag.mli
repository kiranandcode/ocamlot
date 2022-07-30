type t = Types.tag

val create: string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t


val find: string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val find_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t

val self: t -> t Link.t
val name: t -> string
