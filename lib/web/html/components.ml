module H = Tyxml.Html

let build_header ?(title="Strange women lying in ponds distributing swords is no basis for a system of government.!") () =
  let stylesheet href = H.link ~rel:[`Stylesheet] ~href () in
  let charset charset = H.meta ~a:[H.a_charset charset] () in
  let viewport params = H.meta ~a:[H.a_name "viewport"; H.a_content params] () in
  let favicon href = H.link ~rel:[`Icon] ~href () in
  H.head (H.title (H.txt ("OCamlot - " ^ title))) [
    stylesheet "/static/style/style.css";
    favicon "/static/images/logo-1024.png";
    charset "UTF-8";
    viewport "width=device-width, initial-scale=1"
  ]
