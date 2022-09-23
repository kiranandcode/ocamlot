type t = Types.remote_instance

val resolve_instance : int64 -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val create_instance : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val lookup_instance : string -> (module Caqti_lwt.CONNECTION) -> (t option, string) Lwt_result.t
val lookup_instance_exn : string -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val find_possible_remote_instances_to_query: ?offset:int * int -> string -> (module Caqti_lwt.CONNECTION) -> (t list, string) Lwt_result.t
val record_instance_reachable: t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t
val record_instance_unreachable: t -> (module Caqti_lwt.CONNECTION) -> (unit, string) Lwt_result.t

val self : t -> t Link.t
val url : t -> string
val last_unreachable : t -> CalendarLib.Calendar.t option
