module H = Tyxml.Html

let txt = H.txt
let a_class' v = H.a_class [v]
let a_href = H.a_href
let js_script src = H.script ~a:[H.a_mime_type "text/javascript"; H.a_src src] (H.txt "")
let inline_script src = H.script ~a:[H.a_mime_type "text/javascript"] (H.cdata_script src)
let noscript text = H.noscript [txt text]

module B = Bulma.Bulma (struct
    include H
    let figure ?a elts = figure ?a elts
    let table ?a elts = table ?a elts
  end)


