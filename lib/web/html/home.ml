open Common

let form req ~label ~field ~placeholder =
  H.form ~a:[H.a_method `Post] [
    Utils.csrf_tag req;
    H.div ~a:[H.a_class ["field"; "is-horizontal"]] [
      H.div ~a:[H.a_class ["field-label"; "is-normal"]] [
        H.label ~a:[H.a_class ["label"]; H.a_label_for field] [
          H.txt label
        ]
      ];
      H.div ~a:[H.a_class ["field-body"]] [
        H.div ~a:[H.a_class ["field"]] [
          H.input ~a:[
            H.a_class ["input"];
            H.a_input_type `Text;
            H.a_placeholder placeholder;
            H.a_name field;
          ] ()
        ];
        H.input ~a:[H.a_class ["button"; "is-primary"]; H.a_input_type `Submit] ();
      ]
    ]
  ]

let notification ?(classes=[]) elts =
  H.div ~a:[H.a_class ("notification" :: classes)]
    ((H.button ~a:[H.a_class ["delete"]] []) :: elts)

let build_post ?(errors=[]) req =
  H.div ~a:[H.a_class ["tile"]] [
    H.div ~a:[H.a_class ["container"; "hero-body"]] @@ List.concat [
      [H.h1 ~a:[H.a_class ["title"]] [H.txt "Welcome to OCamlot!"]];
      (List.map (fun err -> notification ~classes:["is-danger"; "is-light"] [H.txt err]) errors);
      [
        B.level [
          form req ~label:"Toast: " ~field:"post" ~placeholder:"Post a toast to OCamelot"
        ];
        B.level [
          form req ~label:"Follow: " ~field:"follow" ~placeholder:"username@domain.tld"
        ];
      ]]]

let build_posts_list (posts : Database.Post.t list) =
  let module DP = Database.Post in
  Fun.flip List.map posts @@ fun post ->
  B.media ~left:[
    B.image ~a_class:["is-64x64"]
      ~src:"https://ocamlot.xyz/images/avi.png"
      ~alt:"Example username" ()
  ] [
    H.div ~a:[H.a_class ["content"; "is-max-desktop"]] [
      H.p [(H.txt (DP.summary post |> Option.value ~default:""))];
      H.a ~a:[H.a_href (DP.url post)] [
        H.txt @@ (DP.published post
                  |> CalendarLib.Printer.Calendar.to_string)];
      H.br ();
      H.p [H.txt @@ DP.post_source post]
    ];
  ]

let build_url config time offset txt incr =
  let url = Configuration.Url.home_url config
            |> Fun.flip Uri.with_query [
              "time", 
              (Ptime.of_float_s
                 (CalendarLib.Calendar.to_unixfloat time))
              |> Option.map (Ptime.to_rfc3339 ~tz_offset_s:0)
              |> Option.to_list ;
              "offset", [Int.to_string (offset + incr)]]
            |> Uri.to_string in
  B.level [
    H.div ~a:[H.a_class ["level-item"]] [
      B.button ~a_class:["is-link"]
        ~a:[H.a_href url]
        txt
    ]
  ]


let body config ~offset:(time,offset) ~errors ~posts (user: Database.LocalUser.t option) req =
  H.body ~a:[H.a_class ["has-navbar-fixed-top"]] @@ List.concat [
    [Navbar.build user req];
    [B.container [H.div ~a:[H.a_class ["tile"; "is-ancestor"]] @@ List.concat [
       Option.map (fun _ -> [build_post ~errors req]) user |> Option.value ~default:[];
       [H.div ~a:[H.a_class ["tile"]]
          [B.container @@ List.concat [
             (match offset with 0 -> [] | _ -> [build_url config time offset "prev" (-1)]);
             [B.container @@ build_posts_list posts];
             (match posts with _ :: _ -> [build_url config time offset "next" 1]  | _ -> []);
           ]]];
     ]]];
    [Navbar.script];
    [noscript "Javascript may be required (but don't worry, it's all Libre my friend!)"]
  ]


let build config ~offset ~errors ~posts user req =
  let head = Components.build_header ~title:"Home" () in
  let body = body config ~offset ~errors ~posts user req in
  Utils.build_document head body
