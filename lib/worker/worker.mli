module Task = Task

val send_task : Task.t -> unit
val init : 'a -> unit Lwt.t
