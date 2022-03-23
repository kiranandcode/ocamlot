open Common

let build config ((follow: Database.Follow.t), (author: Database.Actor.t)) =
  let module DF = Database.Follow in
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
      [H.a ~a:[H.a_href (DF.url follow)] [
         H.txt @@ (DF.created follow
                   |> CalendarLib.Printer.Calendar.to_string)];
       H.br ()];
    ];
  ]
