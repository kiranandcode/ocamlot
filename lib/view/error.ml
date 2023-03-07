open Utils


let render_error err =
  H.div ~a:[H.a_class ["error"]] [
    H.p [H.txt err]
  ]

let render_error_list errors =
  H.div ~a:[H.a_class ["error-list"]] (
    List.map render_error errors
  )
