
let user_base_url config =
  Params.domain config
  |> Fun.flip Uri.with_path "/users"

let user config username =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username)

(* following pleroma approach, if content-type is html, then return profile page of user *)
let user_profile_page config username =
  user config username

let user_inbox config username =
  Params.domain config
  |> Fun.flip Uri.with_path ("/users/" ^ username ^ "/inbox")

let user_outbox config username =
  Params.domain config
  |> Fun.flip Uri.with_path ("/users/" ^ username ^ "/outbox")

let api_base_path config =
  Params.domain config
  |> Fun.flip Uri.with_path "api"
