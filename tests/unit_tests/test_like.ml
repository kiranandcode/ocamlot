open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "like" end)
module Poly = struct let (=) = Containers.Equal.poly end

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

let with_users_and_posts ~users:n ~posts:m f =
  with_users n @@ fun db users ->
  let+ posts =
    Lwt_list.map_s
      (fun i ->
         let* post = 
           Database.Post.create_post
             ~public_id:("as9302390" ^ string_of_int i)
             ~summary:("Re: Testing" ^ string_of_int i)
             ~is_follower_public:false
             ~post_content:`Text
             ~url:("https://localhost.local/activities/" ^ ("as9302390" ^ string_of_int i))
             ~author:users.(min i (n - 1))
             ~is_public:true
             ~post_source:"Does posts works?"
             ~published:(CalendarLib.Calendar.now ()) db in
         Lwt.return post
      ) (List.init m Fun.id) in
  let posts = Array.of_list posts in
  f db users posts
;;


T.add_test "can create like" @@ with_users_and_posts ~users:1 ~posts:1 @@ fun db users posts ->
let+ like =
  Database.Like.create
    ~public_id:"12890409232"
    ~url:"https://localhost.local/posts/12890409232"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
ret begin
  begin match like with
  | Ok _ -> ()
  | Error e -> print_endline e
  end;
  check_is_ok like
end
;;

T.add_test "can find like by url" @@ with_users_and_posts ~users:1 ~posts:1 @@ fun db users posts ->
let url = "https://localhost.local/posts/12890409232" in
let* _ =
  Database.Like.create
    ~public_id:"12890409232"
    ~url
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
let* like = Database.Like.lookup_like_by_url url db in
ret begin
  check_is_some like
end
;;

T.add_test "can find like by public id" @@ with_users_and_posts ~users:1 ~posts:1 @@ fun db users posts ->
let public_id = "12890409232" in
let* _ =
  Database.Like.create
    ~public_id
    ~url:"https://localhost.local/posts/12890409232"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
let* like = Database.Like.lookup_like_by_public_id public_id db in
ret begin
  check_is_some like
end
;;

T.add_test "can find likes by post" @@ with_users_and_posts ~users:3 ~posts:2 @@ fun db users posts ->
let* _ =
  Database.Like.create
    ~public_id:"40930493"
    ~url:"https://localhost.local/posts/12890409232"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ =
  Database.Like.create
    ~public_id:"438904034"
    ~url:"https://localhost.local/posts/12890409231"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(1)
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ =
  Database.Like.create
    ~public_id:"38923893"
    ~url:"https://localhost.local/posts/12890409234"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(1)
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ =
  Database.Like.create
    ~public_id:"3121212"
    ~url:"https://localhost.local/posts/12890409238"
    ~post:Database.Post.(self posts.(1))
    ~actor:users.(1)
    ~published:(CalendarLib.Calendar.now ()) db in
let* likes = Database.Like.collect_likes_for_post (Database.Post.self posts.(0)) db in
ret begin
  check_is_true (List.length likes = 3)
end
;;

T.add_test "can find likes by actor" @@ with_users_and_posts ~users:3 ~posts:3 @@ fun db users posts ->
let* _ =
  Database.Like.create
    ~public_id:"40930493"
    ~url:"https://localhost.local/posts/12890409232"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ =
  Database.Like.create
    ~public_id:"438904034"
    ~url:"https://localhost.local/posts/12890409231"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(1)
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ =
  Database.Like.create
    ~public_id:"38923893"
    ~url:"https://localhost.local/posts/12890409234"
    ~post:Database.Post.(self posts.(1))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
let* _ =
  Database.Like.create
    ~public_id:"3121212"
    ~url:"https://localhost.local/posts/12890409238"
    ~post:Database.Post.(self posts.(2))
    ~actor:users.(0)
    ~published:(CalendarLib.Calendar.now ()) db in
let* user0_likes = Database.Like.collect_likes_for_actor users.(0) db in
let* user1_likes = Database.Like.collect_likes_for_actor users.(1) db in
ret begin
  check_is_true (List.length user0_likes = 3);
  check_is_true (List.length user1_likes = 1)
end
;;

T.add_test "can find likes by actor w. limit" @@ with_users_and_posts ~users:1 ~posts:4 @@ fun db users posts ->
let like1_date = CalendarLib.Calendar.now () in
let like2_date = CalendarLib.Calendar.(add like1_date Period.(day 1)) in
let like3_date = CalendarLib.Calendar.(add like2_date Period.(day 1)) in
let like4_date = CalendarLib.Calendar.(add like3_date Period.(day 1)) in
let query_date = CalendarLib.Calendar.(add like4_date Period.(day 10)) in
let* _ =
  Database.Like.create
    ~public_id:"day1"
    ~url:"https://localhost.local/posts/day1"
    ~post:Database.Post.(self posts.(0))
    ~actor:users.(0)
    ~published:(like1_date) db in
let* _ =
  Database.Like.create
    ~public_id:"day2"
    ~url:"https://localhost.local/posts/day2"
    ~post:Database.Post.(self posts.(1))
    ~actor:users.(0)
    ~published:like2_date db in
let* _ =
  Database.Like.create
    ~public_id:"day3"
    ~url:"https://localhost.local/posts/day3"
    ~post:Database.Post.(self posts.(2))
    ~actor:users.(0)
    ~published:like3_date db in
let* _ =
  Database.Like.create
    ~public_id:"day4"
    ~url:"https://localhost.local/posts/day4"
    ~post:Database.Post.(self posts.(3))
    ~actor:users.(0)
    ~published:like4_date db in

let* query_range1_likes =
  Database.Like.collect_likes_for_actor ~offset:(query_date, 1, 0) users.(0) db in
let* query_range2_likes =
  Database.Like.collect_likes_for_actor ~offset:(query_date, 1, 1) users.(0) db in
let* query_range3_likes =
  Database.Like.collect_likes_for_actor ~offset:(query_date, 1, 2) users.(0) db in
let* query_range4_likes =
  Database.Like.collect_likes_for_actor ~offset:(query_date, 1, 3) users.(0) db in
let* query_range5_likes =
  Database.Like.collect_likes_for_actor ~offset:(query_date, 1, 4) users.(0) db in
ret begin
  check_is_true (List.length query_range1_likes = 1);
  check_is_true Poly.(Database.Like.public_id
                        (List.hd query_range1_likes) = Some "day4");

  check_is_true (List.length query_range2_likes = 1);
  check_is_true Poly.(Database.Like.public_id
                        (List.hd query_range2_likes) = Some "day3");
  check_is_true (List.length query_range3_likes = 1);
  check_is_true Poly.(Database.Like.public_id
                        (List.hd query_range3_likes) = Some "day2");
  check_is_true (List.length query_range4_likes = 1);
  check_is_true Poly.(Database.Like.public_id
                        (List.hd query_range4_likes) = Some "day1");
  check_is_true (List.length query_range5_likes = 0);
end
;;




let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
