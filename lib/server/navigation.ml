open Common

let build_navigation_bar req =
  let* user = current_user req in
  return_ok @@ match user with
  | None ->
    View.Utils.(
      [{text="Feed"; url="/feed"; form=None};
       {text="Users"; url="/users"; form=None};
       {text="Log in/Register"; url="/login"; form=None}],
      None
    )
  | Some user ->
    View.Utils.(List.concat [
        [{text="Write"; url="/write"; form=None}];
        [{text="Feed"; url="/feed"; form=None}];
        [{text="Profile"; url="/users/" ^ user.Database.LocalUser.username; form=None}];
        [{text="Follow Reqs."; url="/follow-requests" ^ user.Database.LocalUser.username; form=None}];
        [{text="Users"; url="/users"; form=None}];
        if user.is_admin then [{text="Settings"; url="/settings"; form=None}] else []
      ],
        Some {text="Log out"; url="/logout"; form=None})
