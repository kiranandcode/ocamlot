let log = Logging.add_logger "config"

module Regex = Regex
module Url = Url
module Format = Format
module Features = Features

include Params

let dump_json ?ty (js: Yojson.Safe.t) : unit =
  match Lazy.force dump_json_dir with
  | None -> ()
  | Some dir ->
    let (let*) x f = match x with Ok v -> f v | Error (`Msg err) -> log.warning (fun f -> f "failed to dump json with error %s" err) in
    let* dir = Fpath.of_string dir in
    let time = Ptime.to_rfc3339 (Ptime_clock.now ()) in
    let fname = time ^ (Option.value ~default:"unknown" ty) ^ ".json" in
    let path = Fpath.(dir / fname) in
    let* () = Bos.OS.File.write path (Yojson.Safe.pretty_to_string js) in
    ()

let dump_string ?ty string : unit =
  match Lazy.force dump_json_dir with
  | None -> ()
  | Some dir ->
    let (let*) x f = match x with Ok v -> f v | Error (`Msg err) -> log.warning (fun f -> f "failed to dump json with error %s" err) in
    let* dir = Fpath.of_string dir in
    let time = Ptime.to_rfc3339 (Ptime_clock.now ()) in
    let fname = time ^ (Option.value ~default:"unknown" ty) ^ ".json" in
    let path = Fpath.(dir / fname) in
    let* () = Bos.OS.File.write path string in
    ()
