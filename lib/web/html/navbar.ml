open Common

let script =
  inline_script {|
document.addEventListener('DOMContentLoaded', () => {

  // Get all "navbar-burger" elements
  const $navbarBurgers = Array.prototype.slice.call(document.querySelectorAll('.navbar-burger'), 0);

  // Check if there are any navbar burgers
  if ($navbarBurgers.length > 0) {

    // Add a click event on each of them
    $navbarBurgers.forEach( el => {
      el.addEventListener('click', () => {

        // Get the target from the "data-target" attribute
        const target = el.dataset.target;
        const $target = document.getElementById(target);

        // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
        el.classList.toggle('is-active');
        $target.classList.toggle('is-active');

      });
    });
  }

});
 |}

let build user req =
  B.section [
    B.navbar ~a_class:["is-fixed-top"] 
      [
        (B.navbar_brand @@ List.concat [
           begin match user with
           | None -> []
           | Some user ->
             let display_name = Database.LocalUser.display_name user in
             let profile_path = Printf.sprintf "/user/%s" (Database.LocalUser.username user) in
             [B.navbar_item_a ~a:[H.a_href profile_path] [
                H.label ~a:[H.a_class ["title"]] [H.txt display_name]
              ]]
           end; [
             B.navbar_item_a ~a_class:["is-flex-grow-1"] ~a:[H.a_href "/home"] [

               B.image ~a_class:[ "is-logo" ]
                 ~src:"/static/images/logo-with-text.svg"
                 ~alt:"OCamlot Logo" ();
               (* H.label ~a:[H.a_class ["title"; "has-text-centered"; "is-flex-grow-1"]]
                *   [H.txt "OCamlot"] *)
             ];
             B.navbar_burger ~a:[H.a_user_data "target" "nav-main-menu"] ()
           ]
         ]);
        B.navbar_menu ~a:[H.a_id "nav-main-menu"] ([
          B.navbar_item_a ~a:[H.a_href "/home"] [H.txt "Local timeline"];
          B.navbar_item_a ~a:[H.a_href "/global"] [H.txt "Global timeline"];
        ], [
            B.navbar_item [
              H.div ~a:[H.a_class ["buttons"]] (match user with
                | Some _ -> [
                    H.form ~a:[H.a_action "/logout"; H.a_method `Post] [
                      Utils.csrf_tag req;
                      H.input ~a:[H.a_class ["button"; "is-outlined"; "is-inverted"; "is-danger"];
                                  H.a_value "Log-out"; H.a_input_type `Submit] ();
                    ]
                  ]                  
                | None ->
                  [
                    B.button ~a:[H.a_href "/register"] ~a_class:["is-primary"] "Sign-up";
                    B.button ~a:[H.a_href "/login"] ~a_class:[] "Log in";
                  ])
            ];
          ])
      ]
  ]
