module type TASK = sig
  type t
  type state
  type config

  val perform: config -> state -> t -> unit Lwt.t
  val init_state: config -> state Lwt.t
end


module Make : functor (S: TASK) -> sig
  val init: ?no_workers:int -> S.config -> unit
  val send: S.t -> unit
end
