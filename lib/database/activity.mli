type t = Types.activity
type id

val id: t -> id
val data: t -> Yojson.Safe.t

val url: _ Configuration.Params.t -> id -> Uri.t


val id_from_string: string -> id option
val id_to_string: id -> string

val fresh_id : unit -> id

val create: id:id -> data:Yojson.Safe.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val find: id -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val find_exn : id -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val update: t -> Yojson.Safe.t -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
