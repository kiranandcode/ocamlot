type t = Local of Local_user.t | Remote of Remote_user.t

val resolve: int64 -> (module Caqti_lwt.CONNECTION) -> (t, string) Lwt_result.t
val of_local: Local_user.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t Link.t, string) Lwt_result.t
val of_remote: Remote_user.t Link.t -> (module Caqti_lwt.CONNECTION) -> (t Link.t, string) Lwt_result.t
