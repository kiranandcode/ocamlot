module Common : sig
  module Ty = Caqti_type.Std
  module type DB = Caqti_lwt.CONNECTION

  val ( let+ ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  val ( let* ) : ('a, 'b) result Lwt.t -> ('a -> 'c Lwt.t) -> 'c Lwt.t
  val ret : 'a -> 'a Lwt.t

  val check_is_true : bool -> unit
  val check_is_false : bool -> unit
  val check_is_ok : ('a, 'b) result -> unit
  val check_is_some : 'a option -> unit
  val check_string_eq : expected:string -> string -> unit
  val check_string_neq : expected:string -> string -> unit

  val with_db : ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> unit -> 'a Lwt.t

end


val run : string -> unit

module Make : functor (S : sig val name : string end) -> sig
  val add_test : string -> (unit -> unit) -> unit
  val add_slow_test : string -> (unit -> unit) -> unit
  val run : unit -> unit
end

module Lwt : sig
  val run : string -> unit

  module Make : functor (S : sig val name : string end) -> sig
      val add_test : string -> (unit -> Alcotest_lwt.return) -> unit
      val add_slow_test : string -> (unit -> Alcotest_lwt.return) -> unit
      val run : unit -> unit
  end
end
