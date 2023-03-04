open Common

let build_navigation_bar req =
  let* user = current_user req in
  let login, last =
    match user with
    | None -> View.Utils.([
      {text="Log in"; url="/login"};
      ],
        {text="Register"; url="/register"}
    )
    | Some user ->
      View.Utils.([
        {text="Profile";
         url="/users/" ^ user.Database.LocalUser.username}
      ], {text="Log out"; url="/logout"}) in
  let navigation_components = View.Utils.[
    {text="Feed"; url="/feed"};
    {text="Users"; url="/users"};
  ] in
  return_ok (navigation_components @ login, Some last)
