open Common

let build_navigation_bar req =
  let* user = current_user req in
  let login =
    match user with
    | None -> [
        "Log in", "/login", false;
        "Register", "/register", false
      ]
    | Some user -> View.[
        "Profile", "/users/" ^ user.Database.LocalUser.username, false;
        "Log out", "/logout", true
      ] in
  let navigation_components = [
    "Feed", "/feed", false;
    "Users", "/users", false;
  ] in
  return_ok (navigation_components @ login)
