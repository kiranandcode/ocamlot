open Common

let build config ((author: Database.Actor.t), post) =
  let module DP = Database.Post in
  B.media ~left:[
    B.image ~a_class:["is-64x64"]
      ~src:"https://ocamlot.xyz/images/avi.png"
      ~alt:"Example username" ()
  ] [
    H.div ~a:[H.a_class ["content"; "is-max-desktop"]] @@ List.concat [
      [(match author with
       | Database.Actor.Local l ->
         H.a ~a:[H.a_href (
           Database.LocalUser.username l
           |> Configuration.Url.user config
           |> Uri.to_string
         )]
           [H.txt (Database.LocalUser.display_name l)]
       | Database.Actor.Remote r ->
         H.a ~a:[H.a_href (Database.RemoteUser.url r)] [
           H.txt (Database.RemoteUser.display_name r)
         ]
      )];
      [H.a ~a:[H.a_href (DP.url post)] [
        H.txt @@ (DP.published post
                  |> CalendarLib.Printer.Calendar.to_string)];
       H.br ()];
      match DP.summary post with
      | None ->
        [H.p [H.txt @@ DP.post_source post]]
      | Some summary ->
        [H.p [(H.txt summary)];
        H.br ();
        H.p [H.txt @@ DP.post_source post]]
    ];
  ]
