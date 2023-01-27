
let home_url =
  lazy begin
    Lazy.force Params.domain
    |> Fun.flip Uri.with_path "/home"
  end

let image_path path = "/images/" ^ path

let user_base_url =
  lazy begin
    Lazy.force Params.domain
    |> Fun.flip Uri.with_path "/users"
  end

let user_path username = ("/users/" ^ username)
let user username = 
  Lazy.force Params.domain
  |> Fun.flip  Uri.with_path (user_path username)

let user_followers_path username = ("/users/" ^ username ^ "/followers")
let user_followers username =
  Lazy.force Params.domain
  |> Fun.flip  Uri.with_path (user_followers_path username)

let user_followers_page username ~start_time ~offset =
  Lazy.force Params.domain
  |> Fun.flip  Uri.with_path (user_followers_path username)
  |> Fun.flip Uri.with_query' ["page", offset;
                               "start", start_time ]

let user_following_path username = ("/users/" ^ username ^ "/following")
let user_following username =
  Lazy.force Params.domain
  |> Fun.flip  Uri.with_path (user_following_path username)

let user_following_page username ~start_time ~offset =
  Lazy.force Params.domain
  |> Fun.flip  Uri.with_path (user_following_path username)
  |> Fun.flip Uri.with_query' ["page", offset;
                               "start", start_time ]

let user_key username =
  Lazy.force Params.domain
  |> Fun.flip  Uri.with_path (user_path username)
  |> Fun.flip Uri.with_fragment (Some "main-key")

(* following pleroma approach, if content-type is html, then return profile page of user *)
let user_profile_page username =
  user username

let user_inbox_path username = ("/users/" ^ username ^ "/inbox")
let user_inbox username =
  Lazy.force Params.domain
  |> Fun.flip Uri.with_path (user_inbox_path username)

let user_outbox_path username = ("/users/" ^ username ^ "/outbox")
let user_outbox username =
  Lazy.force Params.domain
  |> Fun.flip Uri.with_path (user_outbox_path username)

let activity_path id = ("/activity/" ^ id)
let activity_base_endpoint = lazy begin
  Lazy.force Params.domain
  |> Fun.flip Uri.with_path ("/activity")
end


let activity_endpoint id =
  Lazy.force Params.domain
  |> Fun.flip Uri.with_path (activity_path id)

let api_base_path = lazy begin
  Lazy.force Params.domain
  |> Fun.flip Uri.with_path "api"
end
