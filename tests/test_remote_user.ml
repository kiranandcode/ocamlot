open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "remote user" end);;
let (>|=) x f = Lwt_result.map f x
let (>>=) x f = Lwt_result.bind x f
;;

T.add_test "can create remote user" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ user =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount" ~public_key_pem:""
    db in
ret begin
  check_is_ok user
end
;;

T.add_test "can lookup remote user by address" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ _ =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"  ~public_key_pem:""
    db in
let+ user = Database.RemoteUser.lookup_remote_user_by_address ~username:"atestaccount" ~domain:"ocamlot.xyz" db in
ret begin
  check_is_ok user
end
;;

T.add_test "can lookup remote user by address successfully" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ _ =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"  ~public_key_pem:""
    db in
let* user = Database.RemoteUser.lookup_remote_user_by_address ~username:"atestaccount" ~domain:"ocamlot.xyz" db in
ret begin
  check_is_some user
end
;;

T.add_test "can lookup remote user by address" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ instance2 = Database.RemoteInstance.create_instance "ocamlot.nfshost.com" db in
let instance2 = Result.get_ok instance2 in

let+ _ =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"  ~public_key_pem:""
    db in
let+ _ =
  Database.RemoteUser.create_remote_user
    ~username:"example"
    ~instance:(Database.RemoteInstance.self instance2)
    ~url:"https://ocamlot.nfshost.com/users/example"  ~public_key_pem:""
    db in

let* user = Database.RemoteUser.lookup_remote_user_by_address ~username:"atestaccount" ~domain:"ocamlot.xyz" db in
let user = Option.get user in
ret begin
  check_string_eq
    ~expected:"https://ocamlot.xyz/users/atestaccount"
    (Database.RemoteUser.url user)
end
;;

T.add_test "can get all known remote hosts" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ instance2 = Database.RemoteInstance.create_instance "ocamlot.nfshost.com" db in
let instance2 = Result.get_ok instance2 in

let* _ =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"  ~public_key_pem:""
    db in
let* _ =
  Database.RemoteUser.create_remote_user
    ~username:"example"
    ~instance:(Database.RemoteInstance.self instance2)
    ~url:"https://ocamlot.nfshost.com/users/example"  ~public_key_pem:""
    db in
let+ user = Database.RemoteUser.get_known_remote_actors db in
ret begin
  check_is_ok user
end
;;

T.add_test "can get all known remote hosts correctly" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ instance2 = Database.RemoteInstance.create_instance "ocamlot.nfshost.com" db in
let instance2 = Result.get_ok instance2 in

let+ _ =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"  ~public_key_pem:""
    db in
let+ _ =
  Database.RemoteUser.create_remote_user
    ~username:"example"
    ~instance:(Database.RemoteInstance.self instance2)
    ~url:"https://ocamlot.nfshost.com/users/example"  ~public_key_pem:""
    db in
let* remote_users = Database.RemoteUser.get_known_remote_actors db in
ret begin
Alcotest.(check (list (triple string string string))) "returned users"
  [("atestaccount", "ocamlot.xyz",
                "https://ocamlot.xyz/users/atestaccount");
               ("example", "ocamlot.nfshost.com",
                "https://ocamlot.nfshost.com/users/example")]
  remote_users
end
;;


T.add_test "can get all following remote users" @@ with_db @@ fun db ->
let* instance1 = Database.RemoteInstance.create_instance "ocamlot.xyz" db
              >|= Database.RemoteInstance.self in
let* instance2 = Database.RemoteInstance.create_instance "ocamlot.nfshost.com" db
              >|= Database.RemoteInstance.self in
let* lu =
  Database.LocalUser.create_user
    ~username:"example" ~password:"johnyylongjohns" db
    >|= Database.LocalUser.self in
let* lu' = Fun.flip Database.Actor.of_local db lu in

let* ru =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser1" ~instance:instance1 ~public_key_pem:""
    ~url:"https://ocamlot.xyz/users/remoteuser1" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* ru2 =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser2" ~instance:instance2  ~public_key_pem:""
    ~url:"https://ocamlot.nfshost.com/users/remoteuser2" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* ru3 =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser3" ~instance:instance2  ~public_key_pem:""
    ~url:"https://ocamlot.nfshost.com/users/remoteuser3" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* _ =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser4" ~instance:instance2  ~public_key_pem:""
    ~url:"https://ocamlot.nfshost.com/users/remoteuser4" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* _ =
  Database.Follow.create_follow ~url:"followru1" ~author:ru ~target:lu'
    ~pending:false ~created:(CalendarLib.Calendar.now ())
    db in
let* _ =
  Database.Follow.create_follow ~url:"followru2" ~author:ru2 ~target:lu'
    ~pending:false ~created:(CalendarLib.Calendar.now ())
    db in
let* _ =
  Database.Follow.create_follow ~url:"followru3" ~author:ru3 ~target:lu'
    ~pending:true ~created:(CalendarLib.Calendar.now ())
    db in
    
let* remote_users = Database.RemoteUser.collect_remote_users_following 
                      lu db in
ret begin
  let pp_ru ru = Database.RemoteUser.username ru in
  let check_result exp ls = Alcotest.(check (list string)) "remote users match"
                       exp (List.map pp_ru ls) in

  check_result ["remoteuser1"; "remoteuser2"] remote_users
end
;;


T.add_test "can get all following remote users w. limit" @@ with_db @@ fun db ->
let* instance1 = Database.RemoteInstance.create_instance "ocamlot.xyz" db
              >|= Database.RemoteInstance.self in
let* instance2 = Database.RemoteInstance.create_instance "ocamlot.nfshost.com" db
              >|= Database.RemoteInstance.self in
let* lu =
  Database.LocalUser.create_user
    ~username:"example" ~password:"johnyylongjohns" db
    >|= Database.LocalUser.self in
let* lu' = Fun.flip Database.Actor.of_local db lu in

let* ru =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser1" ~instance:instance1 ~public_key_pem:""
    ~url:"https://ocamlot.xyz/users/remoteuser1" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* ru2 =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser2" ~instance:instance2  ~public_key_pem:""
    ~url:"https://ocamlot.nfshost.com/users/remoteuser2" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* ru3 =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser3" ~instance:instance2  ~public_key_pem:""
    ~url:"https://ocamlot.nfshost.com/users/remoteuser3" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* _ =
  Database.RemoteUser.create_remote_user
    ~username:"remoteuser4" ~instance:instance2  ~public_key_pem:""
    ~url:"https://ocamlot.nfshost.com/users/remoteuser4" db
  >|= Database.RemoteUser.self
  >>= Fun.flip Database.Actor.of_remote db in
let* _ =
  Database.Follow.create_follow ~url:"followru1" ~author:ru ~target:lu'
    ~pending:false ~created:(CalendarLib.Calendar.now ())
    db in
let* _ =
  Database.Follow.create_follow ~url:"followru2" ~author:ru2 ~target:lu'
    ~pending:false ~created:(CalendarLib.Calendar.now ())
    db in
let* _ =
  Database.Follow.create_follow ~url:"followru3" ~author:ru3 ~target:lu'
    ~pending:true ~created:(CalendarLib.Calendar.now ())
    db in
    
let* remote_users = Database.RemoteUser.collect_remote_users_following ~offset:(2,1)
                      lu db in
ret begin
  let pp_ru ru = Database.RemoteUser.username ru in
  let check_result exp ls = Alcotest.(check (list string)) "remote users match"
                       exp (List.map pp_ru ls) in

  check_result ["remoteuser2"] remote_users
end
;;


let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
