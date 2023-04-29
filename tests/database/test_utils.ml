let () = Printexc.register_printer (function (Failure msg) -> Some msg | _ -> None)

let run f =
  if Array.length Sys.argv <= 1 then ()
  else begin
    let db_name = Sys.argv.(1) in
    let args = List.init (Array.length Sys.argv - 2) (fun i -> Sys.argv.(i + 2))  in
    let match_failed = ref false in
    let res = try Lwt_main.run begin
      let _ =  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna) in
      Caqti_lwt.with_connection (Uri.of_string ("sqlite3://:" ^ db_name)) (fun db ->
        f db args
      )
    end
      with
        Match_failure _ -> (match_failed := true; Ok ()) in
    match res with
    | Ok () when not !match_failed -> ()
    | Ok () -> failwith ("unsupported arguments: [" ^ (String.concat "] [" args) ^ "]")
    | Error (#Caqti_error.t as msg) -> failwith (Caqti_error.show msg)
    | Error (`Msg msg) -> failwith msg
    | Error _ -> failwith "unknown error"
  end

