open Utils

let charset charset = H.meta ~a:[H.a_charset charset] ()
let meta name content = H.meta ~a:[H.a_name name; H.a_content content] ()
let stylesheet url = H.link ~rel:[`Stylesheet] ~href:url ()

let render_page title contents =
  H.html ~a:[H.a_class ["theme"]; H.a_lang "en"]
    (H.head (H.title (H.txt ("OCamlot - " ^ title))) [
        charset "utf-8";
        meta "viewport" "width=device-width, initial-scale=1.0";
        meta "generator" "tyxml";
        stylesheet "/static/style/colors.css";
        stylesheet "/static/style/fonts.css";
        stylesheet "/static/style/components.css";
        stylesheet "/static/style/style.css";
      ]) (H.body [
        div "main-body" contents
      ])
