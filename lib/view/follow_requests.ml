open Utils

type t = {
  user: User.t;
  date: Ptime.t;
  actions: link list;
}

let render_follow_request request =
  User.render_user_box ~a_class:["follow-request"; "font-small"; "flex"] request.user
    (H.div ~a:[H.a_class ["follow-request-panel";"justify-space-around";"flex-column"]] [
        H.div ~a:[H.a_class ["request-actions"; "elements-list"]]
          (List.map (fun link -> H.a ~a:[H.a_href link.url] [H.txt link.text])
             request.actions
           |> intersperse (fun () -> H.p [H.txt "|"]));
        H.div ~a:[H.a_class ["request-date"]] [
          H.i [H.txt (Format.asprintf "Posted on %a" pp_date (Ptime.to_date request.date))]
        ]
      ])

let render_follow_requests_grid requests =
  H.div ~a:[H.a_class ["grid"; "follow-requests-grid"]]
    (List.map render_follow_request requests)
