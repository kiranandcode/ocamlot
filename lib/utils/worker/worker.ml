open Containers

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
end = functor (S: TASK) -> struct

  let (let+) x f = Lwt.bind x f
  let rec worker config state sock =
    let+ result = Zmq_lwt.Socket.recv sock in
    let task : S.t = Marshal.from_string result 0 in
    let+ () = S.perform config state task in
    worker config state sock

  let children = ref []
  let send_socket = ref None

  let worker_id = 
    "ipc://workers" ^ (string_of_int @@ Unix.getpid ())

  let spawn_worker config worker_id =
    match Unix.fork () with
    | 0 ->
      let ctx = Zmq.Context.create () in
      let socket = Zmq.Socket.create ctx Zmq.Socket.dealer in
      Zmq.Socket.connect socket worker_id;
      ignore @@ Lwt_main.run (
        let+ state = S.init_state config in
        worker config state Zmq_lwt.Socket.(of_socket socket));
      exit 0
    | child -> children := child :: !children; ()

  let init ?(no_workers=1) config =
    for _ = 1 to no_workers do
      spawn_worker config worker_id
    done;
    let _ = at_exit (fun () ->
      List.iter (fun child_pid ->
        print_endline @@ "killing child " ^ string_of_int child_pid;
        Unix.kill child_pid Sys.sigkill
      ) !children
    ) in

    let () =
      let ctx = Zmq.Context.create () in
      let socket = Zmq.Socket.create ctx Zmq.Socket.dealer in
      Zmq.Socket.bind socket worker_id;
      send_socket := Some (Zmq_lwt.Socket.of_socket socket) in
    ()

  let send (t: S.t) : unit =
    let ssock = !send_socket |> Option.get_exn_or "workers have not been initialised" in
    Lwt.async (fun () ->
      Zmq_lwt.Socket.send ssock
        (Marshal.to_string t [Marshal.Closures])
    )
end
