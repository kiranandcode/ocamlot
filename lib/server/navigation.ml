open Common

let build_navigation_bar req =
  let+ user = current_user req in
  let login =
    match user with
    | None -> [ "Log in", "/login", false; "Register", "/register", false ]
    | Some _ -> [ "Profile", "/profile", false; "Log out", "/logout", true] in
  let navigation_components = ["feed", "/feed", false] in
  return_ok (navigation_components @ login)
