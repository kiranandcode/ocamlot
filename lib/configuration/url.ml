

let home_url config =
  Params.domain config
  |> Fun.flip Uri.with_path "/home"


let user_base_url config =
  Params.domain config
  |> Fun.flip Uri.with_path "/users"

let user config username =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username)

let user_followers config username =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username ^ "/followers")

let user_followers_page config username ~start_time ~offset =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username ^ "/followers")
  |> Fun.flip Uri.with_query' ["page", offset;
                               "start", start_time ]

let user_following  config username =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username ^ "/following")

let user_following_page config username ~start_time ~offset =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username ^ "/following")
  |> Fun.flip Uri.with_query' ["page", offset;
                               "start", start_time ]

let user_key config username =
  Params.domain config
  |> Fun.flip  Uri.with_path ("/users/" ^ username)
  |> Fun.flip Uri.with_fragment (Some "main-key")

(* following pleroma approach, if content-type is html, then return profile page of user *)
let user_profile_page config username =
  user config username

let user_inbox config username =
  Params.domain config
  |> Fun.flip Uri.with_path ("/users/" ^ username ^ "/inbox")

let user_outbox config username =
  Params.domain config
  |> Fun.flip Uri.with_path ("/users/" ^ username ^ "/outbox")

let activity_base_endpoint config =
  Params.domain config
  |> Fun.flip Uri.with_path ("/activity")


let activity_endpoint config id =
  Params.domain config
  |> Fun.flip Uri.with_path ("/activity/" ^ id)

let api_base_path config =
  Params.domain config
  |> Fun.flip Uri.with_path "api"
