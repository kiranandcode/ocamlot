open Common

let build_navigation_bar req =
  let* user = current_user req in
  return_ok @@ match user with
  | None ->
    View.Utils.(
      [{text="Feed"; url="/feed"};
       {text="Users"; url="/users"}],
      Some {text="Log in/Register"; url="/login"}
    )
  | Some user ->
    View.Utils.(List.concat [
        [{text="Write"; url="/write"}];
        [{text="Feed"; url="/feed"}];
        [{text="Profile"; url="/users/" ^ user.Database.LocalUser.username}];
        [{text="Follow Reqs."; url="/follow-requests" ^ user.Database.LocalUser.username}];
        [{text="Users"; url="/users"}];
        if user.is_admin then [{text="Settings"; url="/settings"}] else []
      ],
        Some {text="Log out"; url="/logout"}) 
