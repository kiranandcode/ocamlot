open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "post" end)
module Poly = struct let (=) = Containers.Equal.poly end

let with_users n f =
  with_db @@ fun db ->
  let+ users =
    Lwt_list.map_s
      (fun i ->
         let* user = 
           Database.LocalUser.create_user
             ~username:("a-user" ^ string_of_int i)
             ~password:"areallygoodpasswordhere121" db in
         let* user = Database.Actor.of_local user db in
         Lwt.return user
      ) (List.init n Fun.id) in
  let users = Array.of_list users in
  f db users
;;

T.add_test "can create post" @@ with_users 1 @@ fun db users ->
let public_id = "129012901290" in
let+ post = 
  Database.Post.create_post
    ~public_id
    ~summary:"Re: Testing"
    ~url:("https://localhost.local/activities/" ^ public_id)
    ~author:users.(0)
    ~is_public:true
    ~post_source:"Does posts works?"
    ~published:(CalendarLib.Calendar.now ()) db in
ret begin
  check_is_ok post
end
;;

T.add_test "post data is accurately captured" @@ with_users 1 @@ fun db users ->
let public_id = "129012901290" in
let published = CalendarLib.Calendar.now () in
let summary = "Re: Testing" in
let post_source = "Does posts works?" in
let url = "https://localhost.local/activities/" ^ public_id in
let is_public = true in
let* post = 
  Database.Post.create_post
    ~public_id
    ~summary
    ~url
    ~author:users.(0)
    ~is_public
    ~post_source
    ~published db in
ret begin
  check_is_true @@ Poly.(Database.Post.public_id post = Some public_id);
  check_is_true @@ Poly.(Database.Post.summary post = Some summary);
  check_is_true @@ Poly.(Database.Post.post_source post = post_source);
  check_is_true @@ Poly.(Database.Post.url post = url);
  check_is_true @@ Poly.(Database.Post.raw_data post = None);
  check_is_true @@ Poly.(Database.Post.is_public post = is_public);
  check_is_true @@ CalendarLib.Calendar.equal (Database.Post.published post) published;
end
;;

T.add_test "posts can be looked up by public_id" @@ with_users 1 @@ fun db users ->
let public_id = "129012901290" in
let published = CalendarLib.Calendar.now () in
let summary = "Re: Testing" in
let post_source = "Does posts works?" in
let url = "https://localhost.local/activities/" ^ public_id in
let is_public = true in
let* _ = 
  Database.Post.create_post
    ~public_id
    ~summary
    ~url
    ~author:users.(0)
    ~is_public
    ~post_source
    ~published db in
let+ post = Database.Post.lookup_post_by_public_id_exn public_id db in
ret begin
  check_is_ok post;
  let post = Result.get_ok post in
  check_is_true @@ Poly.(Database.Post.public_id post = Some public_id);
  check_is_true @@ Poly.(Database.Post.summary post = Some summary);
  check_is_true @@ Poly.(Database.Post.post_source post = post_source);
  check_is_true @@ Poly.(Database.Post.url post = url);
  check_is_true @@ Poly.(Database.Post.is_public post = is_public);

  check_is_true @@ CalendarLib.Calendar.equal (Database.Post.published post) published;

end
;;

T.add_test "posts can be looked up by url" @@ with_users 1 @@ fun db users ->
let public_id = "129012901290" in
let published = CalendarLib.Calendar.now () in
let summary = "Re: Testing" in
let post_source = "Does posts works?" in
let url = "https://localhost.local/activities/" ^ public_id in
let is_public = true in
let* _ = 
  Database.Post.create_post
    ~public_id
    ~summary
    ~url
    ~author:users.(0)
    ~is_public
    ~post_source
    ~published db in
let+ post = Database.Post.lookup_post_by_url_exn url db in
ret begin
  check_is_ok post;
  let post = Result.get_ok post in
  check_is_true @@ Poly.(Database.Post.public_id post = Some public_id);
  check_is_true @@ Poly.(Database.Post.summary post = Some summary);
  check_is_true @@ Poly.(Database.Post.post_source post = post_source);
  check_is_true @@ Poly.(Database.Post.url post = url);
  check_is_true @@ Poly.(Database.Post.is_public post = is_public);

  check_is_true @@ CalendarLib.Calendar.equal (Database.Post.published post) published;

end
;;

T.add_test "multiple posts can be created" @@ with_users 2 @@ fun db users ->
let public_id = "129012901290" in
let published = CalendarLib.Calendar.now () in
let summary = "Re: Testing" in
let post_source = "Does posts works?" in
let url = "https://localhost.local/activities/" ^ public_id in
let is_public = true in

let* post = 
  Database.Post.create_post
    ~public_id
    ~summary
    ~url
    ~author:users.(0)
    ~is_public
    ~post_source
    ~published db in
let* _ = 
  Database.Post.create_post
    ~public_id:"129129102901290"
    ~url:"https://localhost.local/activities/129129102901290"
    ~author:users.(1)
    ~is_public:false
    ~post_source:"Posting multiple bro"
    ~published db in
ret begin
  check_is_true @@ Poly.(Database.Post.public_id post = Some public_id);
  check_is_true @@ Poly.(Database.Post.summary post = Some summary);
  check_is_true @@ Poly.(Database.Post.post_source post = post_source);
  check_is_true @@ Poly.(Database.Post.url post = url);
  check_is_true @@ Poly.(Database.Post.is_public post = is_public);

  check_is_true @@ CalendarLib.Calendar.equal (Database.Post.published post) published;

end
;;

T.add_test "can collect posts by user" @@ with_users 2 @@ fun db users ->

let* _ = 
  Database.Post.create_post
    ~public_id:"129012901290"
    ~summary:"Re: Testing"
    ~url:"https://localhost.local/activities/129012901290"
    ~author:users.(0)
    ~is_public:true
    ~post_source:"Does posts works?"
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ = 
  Database.Post.create_post
    ~public_id:"32322901290"
    ~summary:"Re: Testing2"
    ~url:"https://localhost.local/activities/129012901291"
    ~author:users.(0)
    ~is_public:true
    ~post_source:"Does posts works 2?"
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ = 
  Database.Post.create_post
    ~public_id:"129129102901290"
    ~url:"https://localhost.local/activities/129129102901290"
    ~author:users.(1)
    ~is_public:false
    ~post_source:"Posting multiple bro"
    ~published:(CalendarLib.Calendar.now ()) db in
let+ posts_by_0 = Database.Post.collect_posts_by_author users.(0) db in
let+ posts_by_1 = Database.Post.collect_posts_by_author users.(1) db in
ret begin
  check_is_ok posts_by_0;
  let posts_by_0 = Result.get_ok posts_by_0 in
  check_is_ok posts_by_1;
  let posts_by_1 = Result.get_ok posts_by_1 in
  check_is_true (List.length posts_by_0 = 2);
  check_is_true (List.length posts_by_1 = 1)
end
;;


T.add_test "can add to post to list" @@ with_users 4 @@ fun db users ->
let* post1 = 
  Database.Post.create_post
    ~public_id:"129012901290"
    ~summary:"Re: Testing"
    ~url:"https://localhost.local/activities/129012901290"
    ~author:users.(0)
    ~is_public:true
    ~post_source:"Does posts works?"
    ~published:(CalendarLib.Calendar.now ()) db in
let* post2 = 
  Database.Post.create_post
    ~public_id:"129012901292"
    ~summary:"Re: Testing 2"
    ~url:"https://localhost.local/activities/129012901292"
    ~author:users.(1)
    ~is_public:true
    ~post_source:"Does posts works 2?"
    ~published:(CalendarLib.Calendar.now ()) db in

let* () =
  Database.Post.add_post_tos
    (Database.Post.self post1)
    [users.(1); users.(2)] db in
let* post1_tos =
  Database.Post.post_to (Database.Post.self post1) db in
let* post1_ccs =
  Database.Post.post_cc (Database.Post.self post1) db in

let* () =
  Database.Post.add_post_tos
    (Database.Post.self post2)
    [users.(0); users.(2)] db in
let* () =
  Database.Post.add_post_ccs
    (Database.Post.self post2)
    [users.(1)] db in

let* post2_tos =
  Database.Post.post_to (Database.Post.self post2) db in
let* post2_ccs =
  Database.Post.post_cc (Database.Post.self post2) db in


ret begin
  check_is_true (List.length post1_ccs = 0);
  check_is_true (List.length post1_tos = 2);

  check_is_true (List.length post2_ccs = 1);
  check_is_true (List.length post2_tos = 2);
end
;;

T.add_test "can add tags to a post" @@ with_users 4 @@ fun db users ->
let open Containers in
let* post1 = 
  Database.Post.create_post
    ~public_id:"129012901290"
    ~summary:"Re: Testing"
    ~url:"https://localhost.local/activities/129012901290"
    ~author:users.(0)
    ~is_public:true
    ~post_source:"Does posts works?"
    ~published:(CalendarLib.Calendar.now ()) db in
let* post2 = 
  Database.Post.create_post
    ~public_id:"129012901292"
    ~summary:"Re: Testing 2"
    ~url:"https://localhost.local/activities/129012901292"
    ~author:users.(1)
    ~is_public:true
    ~post_source:"Does posts works 2?"
    ~published:(CalendarLib.Calendar.now ()) db in

let* hello = Database.Tag.create "hello" db
             |> Lwt_result.map Database.Tag.self in
let* goodbye = Database.Tag.create "goodbye" db
               |> Lwt_result.map Database.Tag.self in
let* epic = Database.Tag.create "epic" db
               |> Lwt_result.map Database.Tag.self in
let* sad = Database.Tag.create "sad" db
               |> Lwt_result.map Database.Tag.self  in
let* covefe' = Database.Tag.create "covefe" db
           in
let covefe = Database.Tag.self covefe' in

let* () =
  Database.Post.add_post_tag
    Database.Post.(self post1)
    hello db in
let* () =
  Database.Post.add_post_tag
    Database.Post.(self post1)
    goodbye db in
let* () =
  Database.Post.add_post_tag
    Database.Post.(self post1)
    epic db in

let url = "https://localhost/tags/covefe" in

let* () =
  Database.Post.add_post_tags
    Database.Post.(self post2)
    [sad, None; covefe, Some url] db in

let* post1_tags = Database.Post.collect_post_tags Database.Post.(self post1) db in
let* post2_tags = Database.Post.collect_post_tags Database.Post.(self post2) db in

ret begin
  let module StringSet = Set.Make (String) in
  let pp fmt vl = StringSet.pp String.pp fmt vl in
  let stringset = Alcotest.testable pp StringSet.equal in
  check_is_true (List.length post1_tags = 3);
  check_is_true (List.length post2_tags = 2);

  let post1_tags = 
    List.fold_left (fun set (tag,_) ->
      StringSet.add (Database.Tag.name tag) set)
      StringSet.empty post1_tags in

  let find v =
    List.Assoc.get ~eq:(fun t1 t2 -> String.(Database.Tag.(name t1 = name t2)))
      v post2_tags in

  Alcotest.(check (option (option string))) "post2 url can be found"
    (find covefe') (Some (Some url));
    

  Alcotest.(check stringset) "post1 tags are equal"
    post1_tags (StringSet.of_list [
    "hello"; "goodbye"; "epic"
  ])
end
;;


T.add_test "can add mentions to a post" @@ with_users 4 @@ fun db users ->
let open Containers in
let* post1 = 
  Database.Post.create_post
    ~public_id:"129012901290"
    ~summary:"Re: Testing"
    ~url:"https://localhost.local/activities/129012901290"
    ~author:users.(0)
    ~is_public:true
    ~post_source:"Does posts works?"
    ~published:(CalendarLib.Calendar.now ()) db in
let* post2 = 
  Database.Post.create_post
    ~public_id:"129012901292"
    ~summary:"Re: Testing 2"
    ~url:"https://localhost.local/activities/129012901292"
    ~author:users.(1)
    ~is_public:true
    ~post_source:"Does posts works 2?"
    ~published:(CalendarLib.Calendar.now ()) db in

let* () =
  Database.Post.add_post_mentions
    Database.Post.(self post1)
    [users.(0); users.(1)] db in

let* () =
  Database.Post.add_post_mentions
    Database.Post.(self post2)
    [users.(1); users.(2); users.(3)] db in

let* post1_mentions  =
  Database.Post.collect_post_mentions
    Database.Post.(self post1) db in
let* post2_mentions  =
  Database.Post.collect_post_mentions
    Database.Post.(self post2) db in

ret begin
  check_is_true (List.length post1_mentions = 2);
  check_is_true (List.length post2_mentions = 3);
end
;;



let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
