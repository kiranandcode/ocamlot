module Task : sig
  type t =
      Follow of {
        local : Database.LocalUser.t;
        username : string;
        domain : string;
      }

  type state = Caqti_lwt.connection
  type config = Configuration.Params.t
  val init_state : config -> state Lwt.t
  val perform : config -> state -> t -> unit Lwt.t
end

val lookup_request: string -> (X509.Public_key.t, string) Lwt_result.t

val init : ?no_workers:int -> Task.config -> unit
val send : Task.t -> unit
