open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "post" end)
module Poly = struct let (=) = Containers.Equal.poly end
let (>|=) x f = Lwt_result.map f x
let (>>=) x f = Lwt_result.bind x f

let with_users n f =
  with_db @@ fun db ->
  let+ users =
    Lwt_list.map_s
      (fun i ->
         let* user = 
           Database.LocalUser.create_user
             ~username:("a-user" ^ string_of_int i)
             ~password:"areallygoodpasswordhere121" db 
           >|= Database.LocalUser.self in
         let* user = Database.Actor.of_local user db in
         Lwt.return user
      ) (List.init n Fun.id) in
  let users = Array.of_list users in
  f db users
;;

let with_posts ?sensitive ?dates ~users ~posts:m db f =
  let+ posts =
    Lwt_list.map_s
      (fun i ->
         let* post = 
           Database.Post.create_post
             ~public_id:("post" ^ string_of_int i)
             ~summary:("Re: Testing" ^ string_of_int i)
             ~url:("https://localhost.local/activities/" ^ ("as9302390" ^ string_of_int i))
             ~author:users.(min i ((Array.length users) - 1))
             ~is_public:(match sensitive with
               | None -> true
               | Some sensitive -> not sensitive.(min (Array.length sensitive - 1) i))
             ~post_source:"Does posts works?"
             ~published:(match dates with
               | None -> CalendarLib.Calendar.now ()
               | Some dates -> dates.(min (Array.length dates - 1) i)
             ) db in
         Lwt.return (Database.Post.self post)
      ) (List.init m Fun.id) in
  let posts = Array.of_list posts in
  f posts
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
    ~url:"https://localhost.local/activities/1291291029012901"
    ~author:users.(1)
    ~is_public:true
    ~post_source:"Posting multiple bro"
    ~published:(CalendarLib.Calendar.now ()) db in
let+ posts_by_0 = Database.Post.collect_posts_by_author users.(0) db in
let+ posts_by_1 = Database.Post.collect_posts_by_author users.(1) db in
ret begin
  check_is_ok posts_by_0;
  let posts_by_0 = Result.get_ok posts_by_0 in
  check_is_ok posts_by_1;
  let posts_by_1 = Result.get_ok posts_by_1 in
  check_int_eq ~expected:2 (List.length posts_by_0);
  check_int_eq ~expected:1 (List.length posts_by_1);
end
;;


T.add_test "can add to post's \"to\" list" @@ with_users 4 @@ fun db users ->
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

T.add_test "can collect posts" @@ with_users 5 @@
fun db users ->
let post0_date = CalendarLib.Calendar.now () in
let post1_date = CalendarLib.Calendar.(add post0_date Period.(day 1)) in
let post2_date = CalendarLib.Calendar.(add post1_date Period.(day 1)) in
let post3_date = CalendarLib.Calendar.(add post2_date Period.(day 1)) in
let post4_date = CalendarLib.Calendar.(add post3_date Period.(day 1)) in
let post5_date = CalendarLib.Calendar.(add post4_date Period.(day 1)) in
let _query_date = CalendarLib.Calendar.(add post5_date Period.(day 10)) in
let follow u1 u2 =
  Database.Follow.create_follow
    ~url:("follow" ^
          string_of_int u1 ^
          "->" ^
          string_of_int u2)
    ~created:(CalendarLib.Calendar.now ())
    ~pending:false
    ~author:users.(u1)
    ~target:users.(u2) db in
with_posts ~users
  ~dates:[|post0_date; post1_date; post2_date; post3_date; post4_date|]
  ~sensitive:[|true; false; false; true; true|] ~posts:5 db @@ fun posts ->

let* remote_instance = Database.RemoteInstance.create_instance "testing.com" db
                     >|= Database.RemoteInstance.self in
let* remote_user =
  Database.RemoteUser.create_remote_user
    ~username:"remote0" ~url:"testing.com/users/remote0"
    ~instance:remote_instance ~public_key_pem:"lololol" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in

let* _ = Database.Post.create_post
                ~author:remote_user ~url:"testing.com/posts/post5"
                ~public_id:"post5"
                ~summary:"post 5 by a remote user"
                ~is_public:true ~published:post5_date
                ~post_source:"hell worldd" db
            >|= Database.Post.self in

(* post 0 is by user 0, to user 1, sensitive (should not show up in twkn or local) *)
let* () = Database.Post.add_post_to posts.(0) users.(1) db in
(* post 1 is by user 1, to user 2 not private, but user 0 is following user 1 *)
let* _ = follow 0 1 in
let* () = Database.Post.add_post_to posts.(1) users.(2) db in
(* post 2 is by user 2, to user 3 not private, but user 0 is not following    *)
let* () = Database.Post.add_post_to posts.(2) users.(1) db in
(* post 3 is by user 3, private, but addressed to user 0    *)
let* () = Database.Post.add_post_to posts.(3) users.(0) db in
(* post 4 is by user 4, private, and not addressed to user 0 (should never be seen)  *)
let* () = Database.Post.add_post_to posts.(4) users.(1) db in

(* should be post 1,2,5  *)
let* twkn = Database.Post.collect_post_whole_known_network db in
(* should be post 1,2  *)
let* local = Database.Post.collect_post_local_network db in
(* should be post 0,1,3 *)
let* feed = Database.Post.collect_post_feed users.(0) db in
(* should be post 0, 3 *)
let* direct_messages = Database.Post.collect_post_direct users.(0) db in

ret begin
  let pp_post post = Option.value ~default:"NONE" (Database.Post.public_id post) in
  let check_post_list expected tmln =
    Alcotest.(check (list string))
      "timeline should match"
      expected (List.map pp_post tmln) in
  check_is_true (List.length twkn = 3);
  check_post_list ["post5"; "post2"; "post1"] twkn;
  check_is_true (List.length local = 2);
  check_post_list ["post2"; "post1"] local;
  check_is_true (List.length feed = 3);
  check_post_list ["post3"; "post1"; "post0"] feed;
  check_is_true (List.length direct_messages = 2);
  check_post_list ["post3"; "post0"] direct_messages;
end
;;

T.add_test "can collect posts w. limit" @@ with_users 5 @@
fun db users ->
let post0_date = CalendarLib.Calendar.now () in
let post1_date = CalendarLib.Calendar.(add post0_date Period.(day 1)) in
let post2_date = CalendarLib.Calendar.(add post1_date Period.(day 1)) in
let post3_date = CalendarLib.Calendar.(add post2_date Period.(day 1)) in
let post4_date = CalendarLib.Calendar.(add post3_date Period.(day 1)) in
let post5_date = CalendarLib.Calendar.(add post4_date Period.(day 1)) in
let query_date = CalendarLib.Calendar.(add post5_date Period.(day 10)) in
let follow u1 u2 =
  Database.Follow.create_follow
    ~url:("follow" ^
          string_of_int u1 ^
          "->" ^
          string_of_int u2)
    ~created:(CalendarLib.Calendar.now ())
    ~pending:false
    ~author:users.(u1)
    ~target:users.(u2) db in
with_posts ~users
  ~dates:[|post0_date; post1_date; post2_date; post3_date; post4_date|]
  ~sensitive:[|true; false; false; true; true|] ~posts:5 db @@ fun posts ->

let* remote_instance = Database.RemoteInstance.create_instance "testing.com" db
                     >|= Database.RemoteInstance.self in
let* remote_user =
  Database.RemoteUser.create_remote_user
    ~username:"remote0" ~url:"testing.com/users/remote0"
    ~instance:remote_instance ~public_key_pem:"lololol" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in

let* _ = Database.Post.create_post
                ~author:remote_user ~url:"testing.com/posts/post5"
                ~public_id:"post5"
                ~summary:"post 5 by a remote user"
                ~is_public:true ~published:post5_date
                ~post_source:"hell worldd" db
            >|= Database.Post.self in

(* post 0 is by user 0, to user 1, sensitive (should not show up in twkn or local) *)
let* () = Database.Post.add_post_to posts.(0) users.(1) db in
(* post 1 is by user 1, to user 2 not private, but user 0 is following user 1 *)
let* _ = follow 0 1 in
let* () = Database.Post.add_post_to posts.(1) users.(2) db in
(* post 2 is by user 2, to user 3 not private, but user 0 is not following    *)
let* () = Database.Post.add_post_to posts.(2) users.(1) db in
(* post 3 is by user 3, private, but addressed to user 0    *)
let* () = Database.Post.add_post_to posts.(3) users.(0) db in
(* post 4 is by user 4, private, and not addressed to user 0 (should never be seen)  *)
let* () = Database.Post.add_post_to posts.(4) users.(1) db in

(* should be post [5,2],1  *)
let* twkn = Database.Post.collect_post_whole_known_network
              ~offset:(query_date, 2, 0) db in
(* should be post 2, [1]  *)
let* local =
  Database.Post.collect_post_local_network
    ~offset:(query_date, 2, 1) db in
(* should be post 3,[1,0] *)
let* feed =
  Database.Post.collect_post_feed users.(0)
    ~offset:(post2_date, 3, 0) db in
(* should be post [3, 0] *)
let* direct_messages =
  Database.Post.collect_post_direct
    ~offset:(query_date, 3, 0)
    users.(0) db in

ret begin
  let pp_post post = Option.value ~default:"NONE" (Database.Post.public_id post) in
  let check_post_list expected tmln =
    Alcotest.(check (list string))
      "timeline should match"
      expected (List.map pp_post tmln) in
  check_post_list ["post5"; "post2"] twkn;
  check_post_list ["post1"] local;
  check_post_list ["post1"; "post0"] feed;
  check_post_list ["post3"; "post0"] direct_messages;
end
;;


let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
