
let home_url config =
  Params.domain config
  |> Fun.flip Uri.with_path "/home"

let image_path path = "/images/" ^ path


let user_base_url config =
  Params.domain config
  |> Fun.flip Uri.with_path "/users"

let user_path username = ("/users/" ^ username)
let user config username =
  Params.domain config
  |> Fun.flip  Uri.with_path (user_path username)

let user_followers_path username = ("/users/" ^ username ^ "/followers")
let user_followers config username =
  Params.domain config
  |> Fun.flip  Uri.with_path (user_followers_path username)

let user_followers_page config username ~start_time ~offset =
  Params.domain config
  |> Fun.flip  Uri.with_path (user_followers_path username)
  |> Fun.flip Uri.with_query' ["page", offset;
                               "start", start_time ]

let user_following_path username = ("/users/" ^ username ^ "/following")
let user_following  config username =
  Params.domain config
  |> Fun.flip  Uri.with_path (user_following_path username)

let user_following_page config username ~start_time ~offset =
  Params.domain config
  |> Fun.flip  Uri.with_path (user_following_path username)
  |> Fun.flip Uri.with_query' ["page", offset;
                               "start", start_time ]

let user_key config username =
  Params.domain config
  |> Fun.flip  Uri.with_path (user_path username)
  |> Fun.flip Uri.with_fragment (Some "main-key")

(* following pleroma approach, if content-type is html, then return profile page of user *)
let user_profile_page config username =
  user config username

let user_inbox_path username = ("/users/" ^ username ^ "/inbox")
let user_inbox config username =
  Params.domain config
  |> Fun.flip Uri.with_path (user_inbox_path username)

let user_outbox_path username = ("/users/" ^ username ^ "/outbox")
let user_outbox config username =
  Params.domain config
  |> Fun.flip Uri.with_path (user_outbox_path username)

let activity_path id = ("/activity/" ^ id)
let activity_base_endpoint config =
  Params.domain config
  |> Fun.flip Uri.with_path ("/activity")


let activity_endpoint config id =
  Params.domain config
  |> Fun.flip Uri.with_path (activity_path id)

let api_base_path config =
  Params.domain config
  |> Fun.flip Uri.with_path "api"
