open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "remote user" end);;

T.add_test "can create remote user" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let@ user =
  Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount" ~public_key_pem:""
    db in
check_is_ok user
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
let@ user = Database.RemoteUser.lookup_remote_user_by_address ~username:"atestaccount" ~domain:"ocamlot.xyz" db in
check_is_ok user
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
check_is_some user
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
check_string_eq ~expected:"https://ocamlot.xyz/users/atestaccount" (Database.RemoteUser.url user)
;;

T.add_test "can get all known remote hosts" @@ with_db @@ fun db ->
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

let@ user = Database.RemoteUser.get_known_remote_actors db in
check_is_ok user
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
Alcotest.(check (list (triple string string string))) "returned users"
  [("atestaccount", "ocamlot.xyz",
                "https://ocamlot.xyz/users/atestaccount");
               ("example", "ocamlot.nfshost.com",
                "https://ocamlot.nfshost.com/users/example")]
  remote_users
;;


let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
