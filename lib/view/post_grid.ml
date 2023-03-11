open Utils


let render_post_grid ?redirect_url posts =
  H.div ~a:[H.a_class ["grid"; "post-grid"]]
    (List.map (Post.render ?redirect_url) posts)
