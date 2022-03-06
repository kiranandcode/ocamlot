
let user_tag config username =
  Printf.sprintf "%s@%s" username (Params.domain config |> Uri.host_with_default)

let user_specifier config username =
  Printf.sprintf "acct:%s@%s" username (Params.domain config |> Uri.host_with_default)
