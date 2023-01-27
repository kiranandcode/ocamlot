
let user_tag username =
  Printf.sprintf "%s@%s" username (Lazy.force Params.domain |> Uri.host_with_default)

let user_specifier username =
  Printf.sprintf "acct:%s@%s" username (Lazy.force Params.domain |> Uri.host_with_default)
