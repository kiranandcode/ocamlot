
let run
      ?(workers=[])
      ?(interface =Dream__http.Http.default_interface)
      ?(port = Dream__http.Http.default_port)
      ?(stop = Dream__http.Http.never)
      ?(error_handler = Dream__http.Error_handler.default)
      ?(tls = false)
      ?certificate_file
      ?key_file
      ?(builtins = true)
      ?(greeting = true)
      ?(adjust_terminal = true)
      user's_dream_handler =

  let () = if Sys.unix then
      Sys.(set_signal sigpipe Signal_ignore)
  in

  let adjust_terminal =
    adjust_terminal && Sys.os_type <> "Win32" && Unix.(isatty stderr) in

  let restore_terminal =
    if adjust_terminal then begin
      (* The mystery terminal escape sequence is $(tput rmam). Prefer this,
         hopefully it is portable enough. Calling tput seems like a security
         risk, and I am not aware of an API for doing this programmatically. *)
      prerr_string "\x1b[?7l";
      flush stderr;
      let attributes = Unix.(tcgetattr stderr) in
      attributes.c_echo <- false;
      Unix.(tcsetattr stderr TCSANOW) attributes;
      fun () ->
        (* The escape sequence is $(tput smam). *)
        prerr_string "\x1b[?7h";
        flush stderr
    end
    else
      ignore
  in

  let create_handler signal =
    let previous_signal_behavior = ref Sys.Signal_default in
    previous_signal_behavior :=
      Sys.signal signal @@ Sys.Signal_handle (fun signal ->
        restore_terminal ();
        match !previous_signal_behavior with
        | Sys.Signal_handle f -> f signal
        | Sys.Signal_ignore -> ignore ()
        | Sys.Signal_default ->
          Sys.set_signal signal Sys.Signal_default;
          Unix.kill (Unix.getpid ()) signal)
  in

  create_handler Sys.sigint;
  create_handler Sys.sigterm;

  let log = Dream__http.Http.Log.convenience_log in

  if greeting then begin
    let scheme =
      if tls then
        "https"
      else
        "http"
    in

    begin match interface with
    | "localhost" | "127.0.0.1" ->
      log "Running at %s://localhost:%i" scheme port
    | _ ->
      log "Running on %s:%i (%s://localhost:%i)" interface port scheme port
    end;
    log "Type Ctrl+C to stop"
  end;

  try
    Lwt_main.run begin
      Lwt.join (
        (Dream__http.Http.serve_with_maybe_https
           "run"
           ~interface
           ~port
           ~stop
           ~error_handler
           ~tls:(if tls then `OpenSSL else `No)
           ?certificate_file ?key_file
           ?certificate_string:None ?key_string:None
           ~builtins
           user's_dream_handler) :: workers
      )
    end;
    restore_terminal ()

  with exn ->
    restore_terminal ();
    raise exn
