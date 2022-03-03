open Containers
module H = Tyxml.Html

let csrf_tag req =
  H.input ~a:[
    H.a_name "dream.csrf";
    H.a_value  (Dream.csrf_token req);
    H.a_hidden ()
  ] ()

let to_str t =
  Format.to_string (H.pp ~indent:true ~advert:"Free/Libre Software generated with OCaml" ()) t

let build_document head body =
  to_str @@ H.html ~a:[H.a_xmlns `W3_org_1999_xhtml] head body
