open Utils


let render_post_grid posts =
  H.div ~a:[H.a_class ["grid"; "post-grid"]]
    (List.map Post.render posts)
