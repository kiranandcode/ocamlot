let should_log = ref false

let printf s =
  Format.ksprintf (fun s ->
    if !should_log then  print_endline s else ()
  ) s
