open Containers
open Lwt_result.Syntax

let () =
  Test_utils.run @@ fun db args ->
  let* () = Petrol.VersionedSchema.initialise Database.Tables.db db in
  match[@warning "-8"] args with 
  | "echo":: args -> List.iter print_endline args; Lwt.return_ok ()


  | ["activity"; "create"; id; data] ->
    Database.Activity.create ~id ~data:(Yojson.Safe.from_string data) db

  | ["activity"; "find-by-id"; id] ->
    let* data = Database.Activity.find_by_id ~id db in
    Format.printf "%a" (Option.pp Database.Activity.pp) data;
    Lwt.return_ok ()

  | ["activity"; "update-raw-data"; id; raw_data] ->
    let* () = Database.Activity.update_raw_data ~id ~raw_data:(Yojson.Safe.from_string raw_data) db in
    Lwt.return_ok ()


  (* | ["local-user"; "find-user"] *)
