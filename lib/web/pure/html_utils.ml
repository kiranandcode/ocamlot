module H = Tyxml.Html

let (+::) x f = Some (x :: (Option.value ~default:[] f))
let (!!) = function None -> [] | Some ls -> ls

let css ?a_class vl = H.a_class (vl :: Option.value ~default:[] a_class)
let attr ?a vls = vls @ Option.value ~default:[] a

let mh1 name ?a_class ?a =
  H.h1 ~a:(attr ?a [css ?a_class name])
let mh2 name ?a_class ?a =
  H.h2 ~a:(attr ?a [css ?a_class name])
let mh3 name ?a_class ?a =
  H.h3 ~a:(attr ?a [css ?a_class name])
let mh4 name ?a_class ?a =
  H.h4 ~a:(attr ?a [css ?a_class name])

let ma name ?a_class ?a =
  H.a ~a:(attr ?a [css ?a_class name])
let maside name ?a_class ?a =
  H.aside ~a:(attr ?a [css ?a_class name])
let mdiv name ?a_class ?a =
  H.div ~a:(attr ?a [css ?a_class name])
let mspan name ?a_class ?a =
  H.span ~a:(attr ?a [css ?a_class name])
let mnav name ?a_class ?a =
  H.nav ~a:(attr ?a [css ?a_class name])
let mul name ?a_class ?a =
  H.ul ~a:(attr ?a [css ?a_class name])
let mli name ?a_class ?a =
  H.li ~a:(attr ?a [css ?a_class name])
let mol name ?a_class ?a =
  H.ol ~a:(attr ?a [css ?a_class name])
let mheader name ?a_class ?a =
  H.header ~a:(attr ?a [css ?a_class name])
let mmain name ?a_class ?a =
  H.main ~a:(attr ?a [css ?a_class name])
