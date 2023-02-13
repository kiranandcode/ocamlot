[@@@warning "-32"]
open Containers

let rebuild_timeout = 3.0

(** [is_source s] returns [true] if [s] is a valid source file. *)
let is_source s =
  (String.suffix ~suf:".ml" s ||
   String.suffix ~suf:".mli" s ||
   String.suffix ~suf:".css" s ||
   String.equal s "dune")
  && not (String.contains s '#')

let rebuild_timeout_passed current_time last_rebuild_time =
  let span = Ptime.diff current_time last_rebuild_time in
  (rebuild_timeout <. Ptime.Span.to_float_s span)

let build_project () =
  let open Bos in
  let opam = Cmd.v "opam" in
  match OS.Cmd.run Cmd.(opam % "exec" % "--" % "dune" % "build" % "@server") with
  | Ok () -> true
  | Error (`Msg _) ->
    Format.printf "ERR: failed to build project\n%!";
    false

let args =
  List.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i + 1))

let run_project () =
  let cmd = ["opam"; "exec"; "--";  "dune"; "exec"; "./bin/main.exe"; "--"] @ args in
  let proc =
    UnixLabels.create_process ~stdin:UnixLabels.stdin  ~stdout:UnixLabels.stdout ~stderr:UnixLabels.stderr
      ~prog:"opam" ~args:(Array.of_list cmd) in
  proc

let process_still_running pid =
  match Unix.waitpid [WNOHANG] pid with
  | 0, _  -> true
  | _, Unix.WEXITED 0 -> false
  | _, Unix.WEXITED n ->
    Format.printf "ERR: exited with status %d\n%!" n;
    false
  | _, (Unix.WSIGNALED s | Unix.WSTOPPED s) ->
    Format.printf "ERR: exited with signal %d\n%!" s;
    false

let kill_process pid =
  let open Bos in
  match Unix.waitpid [WNOHANG] pid with
  | 0, _ ->
    Format.printf "INFO: child process running, so will kill\n%!";
    let _ = OS.Cmd.run Cmd.(v "kill" % "-9" % (string_of_int pid)) in
    let _ = Unix.waitpid [] pid in
    Format.printf "INFO: killed child process\n%!";
    ()
  | _ -> ()
  | exception UnixLabels.Unix_error _ -> ()

let inotify = Inotify.create ()

let watch = Inotify.add_watch inotify "./" [Inotify.S_Modify]
let bin_watch = Inotify.add_watch inotify "./bin" [Inotify.S_Modify; Inotify.S_Create]

let js_watch = Inotify.add_watch inotify "./lib/activitypub" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/common" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/configuration" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/database" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/error_handling" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/resolver" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/server" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/utils" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/web" [Inotify.S_Modify; Inotify.S_Create]
let js_watch = Inotify.add_watch inotify "./lib/worker" [Inotify.S_Modify; Inotify.S_Create]

let data_watch = Inotify.add_watch inotify "./static/style" [Inotify.S_Modify; Inotify.S_Create]
let data_watch = Inotify.add_watch inotify "./static/images" [Inotify.S_Modify; Inotify.S_Create]
let data_watch = Inotify.add_watch inotify "./static/fonts" [Inotify.S_Modify; Inotify.S_Create]

let last_rebuild_time = ref None
let running_process_pid = ref None

let rec loop ~should_restart () =
  begin
    let (let+) x f = if x then f () in
    let current_time = Ptime_clock.now () in
    let+ () = Option.for_all (rebuild_timeout_passed current_time)
        !last_rebuild_time in
    (* pessimistically record the current time as the last time we rebuilt *)
    last_rebuild_time := Some current_time;
    Format.printf "INFO: Source changed, rebuilding project\n%!";
    let+ _ = build_project () in
    let current_time = Ptime_clock.now () in
    (* if rebuilding succeeded, update last rebuild time to be new current time  *)
    last_rebuild_time := Some current_time;

    let server_still_running =
      Option.exists process_still_running !running_process_pid in
    if should_restart || not server_still_running then begin
      if should_restart then
        Format.printf "INFO: runtime changed, restarting project\n%!";
      if not server_still_running then
        Format.printf "INFO: old server is dead, restarting project\n%!";

      Option.iter kill_process !running_process_pid;
      running_process_pid := Some (run_project ());
    end
  end;
  let should_restart = ref false in
  let source_change = ref false in

  while not !source_change && not !should_restart do
    let ls = Inotify.read inotify in
    List.iter (fun ((watch, _, _, fname)) ->
      source_change := !source_change || Option.exists is_source fname;
      should_restart := not (Inotify.int_of_watch watch = Inotify.int_of_watch js_watch);
    ) ls;
  done;
  loop ~should_restart:!should_restart ()

let _ = at_exit (fun () -> Option.iter kill_process !running_process_pid)

let () =
  loop ~should_restart:true ()
