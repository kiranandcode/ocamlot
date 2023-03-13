module Task = Task

val send_task : Task.t -> unit
val init : unit -> unit Lwt.t
