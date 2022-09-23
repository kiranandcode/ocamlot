open Bos

let binary_path = Fpath.of_string "../../bin/main.exe" |> Result.get_ok

let () =
  OS.Cmd.run_out Cmd.(v "../../bin/main.exe" % "--help")
  |> OS.Cmd.out_string
  |> Result.get_ok
  |> fst
  |> print_endline

