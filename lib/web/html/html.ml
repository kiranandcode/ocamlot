module Components = Components
module Profile = Profile
module Feed = Feed
module Post = Post
module Login = Login
module Single_post = Single_post
module Write_post = Write_post
module Error = Error


let head title =
  let module H = Tyxml.Html in
  let meta name content =
    H.meta ~a:[H.a_name name; H.a_content content] () in
  let link ?ty rel href =
    H.link ~rel ~href
      ~a:(match ty with
        | None -> []
        | Some ty ->
          [H.a_mime_type ty])
      () in
  H.head (H.title title) [
    H.meta ~a:[H.a_charset "UTF-8"] ();
    meta "viewport" "width=device-width, initial-scale=1";
    meta "theme-color" "#157878";
    link [`Stylesheet] "/static/style/normalize.css";
    link [`Stylesheet] "/static/style/pure.css";
    link [`Stylesheet] "/static/style/grids-responsive-min.css";
    link [`Stylesheet] "/static/style/style.css";
    (* link ~ty:"text/css" [`Stylesheet]
     *   "https://fonts.googleapis.com/css?family=Open+Sans:400,700"; *)
  ]


let build_page ?(details=[]) ?(headers=[]) ?(title="Untitled page...") contents =
  let module H = Tyxml.Html in
  H.html
    (head @@ H.txt (title ^ " - OCamlot"))
    (H.body [
       Components.header headers;
       H.div ~a:[H.a_class ["main"]] contents;
       Components.footer details;
     ])
