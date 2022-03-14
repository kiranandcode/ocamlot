open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "follow" end)
module Poly = struct let (=) = Containers.Equal.poly end
let (>|=) x f = Lwt_result.map f x

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
  f db users;;

T.add_test "can create follow" @@ with_users 2 @@ fun db users ->
let+ follow =
  Database.Follow.create_follow
    ~url:"https://localhost.local/posts/12890409232"
    ~author:users.(0)
    ~target:users.(1)
    ~pending:true
    ~created:(CalendarLib.Calendar.now ()) db in
ret begin
  check_is_ok follow
end
;;

T.add_test "can change follow status" @@ with_users 2 @@ fun db users ->
let* follow =
  Database.Follow.create_follow
    ~url:"https://localhost.local/posts/12890409232"
    ~author:users.(0)
    ~target:users.(1)
    ~pending:true
    ~created:(CalendarLib.Calendar.now ()) db
  >|= Database.Follow.self in
let* () = Database.Follow.update_follow_pending_status follow false db in
let* follow = Database.Link.resolve follow db in
ret begin
  check_is_false (Database.Follow.pending follow)
end
;;

T.add_test "can lookup follow by url" @@ with_users 2 @@ fun db users ->
let url = "https://localhost.local/posts/12890409232" in
let* _ =
  Database.Follow.create_follow
    ~url
    ~author:users.(0)
    ~target:users.(1)
    ~pending:true
    ~created:(CalendarLib.Calendar.now ()) db
  >|= Database.Follow.self in
let* follow = Database.Follow.lookup_follow_by_url url db in
ret begin
  check_is_some follow
end
;;

T.add_test "can lookup follow by public id" @@ with_users 2 @@ fun db users ->
let public_id = "12890409232" in
let* _ =
  Database.Follow.create_follow
    ~public_id
    ~url:"https://localhost.local/posts/12890409232"
    ~author:users.(0)
    ~target:users.(1)
    ~pending:true
    ~created:(CalendarLib.Calendar.now ()) db
  >|= Database.Follow.self in
let* follow = Database.Follow.lookup_follow_by_public_id public_id db in
ret begin
  check_is_some follow
end
;;

T.add_test "can delete follow" @@ with_users 2 @@ fun db users ->
let public_id = "12890409232" in
let url = "https://localhost.local/posts/12890409232" in
let* follow =
  Database.Follow.create_follow
    ~public_id ~url
    ~author:users.(0)
    ~target:users.(1)
    ~pending:true
    ~created:(CalendarLib.Calendar.now ()) db
  >|= Database.Follow.self in
let* () = Database.Follow.delete_follow follow db in
let* follow_id = Database.Follow.lookup_follow_by_public_id public_id db in
let* follow_url = Database.Follow.lookup_follow_by_url url db in
ret begin
  check_is_true (Option.is_none follow_id);
  check_is_true (Option.is_none follow_url)
end
;;

T.add_test "can collect follows for user" @@ with_users 4 @@ fun db users ->
let follow1_date = CalendarLib.Calendar.now () in
let follow2_date = CalendarLib.Calendar.(add follow1_date Period.(day 1)) in
let follow3_date = CalendarLib.Calendar.(add follow2_date Period.(day 10)) in
(* let query_date = CalendarLib.Calendar.(add follow3_date Period.(day 10)) in *)

let* _ =
  Database.Follow.create_follow
    ~public_id:"follow1"
    ~url:"https://localhost.local/posts/follow1"
    ~author:users.(1)
    ~target:users.(0)
    ~pending:true
    ~created:follow1_date db
  >|= Database.Follow.self in
let* _ =
  Database.Follow.create_follow
    ~public_id:"follow2"
    ~url:"https://localhost.local/posts/follow2"
    ~author:users.(2)
    ~target:users.(0)
    ~pending:true
    ~created:follow2_date db in
let* _ =
  Database.Follow.create_follow
    ~public_id:"follow3"
    ~url:"https://localhost.local/posts/follow3"
    ~author:users.(0)
    ~target:users.(3)
    ~pending:true
    ~created:follow3_date db in
let* _ =
  Database.Follow.create_follow
    ~public_id:"followrandom"
    ~url:"https://localhost.local/posts/followrandom"
    ~author:users.(1)
    ~target:users.(2)
    ~pending:true
    ~created:follow3_date db in

let* follows = Database.Follow.collect_follows_for_actor users.(0) db in
ret begin
  let follow_at i =
    Database.Follow.public_id (List.nth follows i) in
  check_is_true (List.length follows = 3);
  
  check_is_true Poly.(follow_at 0 = Some "follow3");
  check_is_true Poly.(follow_at 1 = Some "follow2");
  check_is_true Poly.(follow_at 2 = Some "follow1");
end
;;

T.add_test "can collect follows for user w. offset" @@ with_users 4 @@ fun db users ->
let follow1_date = CalendarLib.Calendar.now () in
let follow2_date = CalendarLib.Calendar.(add follow1_date Period.(day 1)) in
let follow3_date = CalendarLib.Calendar.(add follow2_date Period.(day 10)) in
let query_date = CalendarLib.Calendar.(add follow3_date Period.(day 10)) in

let* _ =
  Database.Follow.create_follow
    ~public_id:"follow1"
    ~url:"https://localhost.local/posts/follow1"
    ~author:users.(1)
    ~target:users.(0)
    ~pending:true
    ~created:follow1_date db
  >|= Database.Follow.self in
let* _ =
  Database.Follow.create_follow
    ~public_id:"follow2"
    ~url:"https://localhost.local/posts/follow2"
    ~author:users.(2)
    ~target:users.(0)
    ~pending:true
    ~created:follow2_date db in
let* _ =
  Database.Follow.create_follow
    ~public_id:"follow3"
    ~url:"https://localhost.local/posts/follow3"
    ~author:users.(0)
    ~target:users.(3)
    ~pending:true
    ~created:follow3_date db in
let* _ =
  Database.Follow.create_follow
    ~public_id:"followrandom"
    ~url:"https://localhost.local/posts/followrandom"
    ~author:users.(1)
    ~target:users.(2)
    ~pending:true
    ~created:follow3_date db in

let* follows_offs_1 =
  Database.Follow.collect_follows_for_actor
    ~offset:(query_date, 1, 0) users.(0) db in
let* follows_offs_2 =
  Database.Follow.collect_follows_for_actor
    ~offset:(query_date, 1, 1) users.(0) db in
let* follows_offs_3 =
  Database.Follow.collect_follows_for_actor
    ~offset:(query_date, 1, 2) users.(0) db in
let* follows_offs_4 =
  Database.Follow.collect_follows_for_actor
    ~offset:(query_date, 1, 3) users.(0) db in

ret begin
  let follow_at ls = Database.Follow.public_id (List.hd ls) in
  check_is_true (List.length follows_offs_1 = 1);
  check_is_true Poly.(follow_at follows_offs_1 = Some "follow3");
  check_is_true (List.length follows_offs_2 = 1);
  check_is_true Poly.(follow_at follows_offs_2 = Some "follow2");
  check_is_true (List.length follows_offs_3 = 1);
  check_is_true Poly.(follow_at follows_offs_3 = Some "follow1");
  check_is_true (List.length follows_offs_4 = 0);
end
;;

let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
