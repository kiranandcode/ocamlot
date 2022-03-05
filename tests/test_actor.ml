open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "actor" end);;

T.add_test "can create remote user" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ user = Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"
    db in
let user = Result.get_ok user in
let@ actor = Database.Actor.of_remote user db in
check_is_ok actor
;;

T.add_test "can create remote user twice" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ user = Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"
    db in
let user = Result.get_ok user in
let+ _ = Database.Actor.of_remote user db in
let@ actor2 = Database.Actor.of_remote user db in
check_is_ok actor2
;;

T.add_test "can resolve remote users" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ user = Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"
    db in
let user = Result.get_ok user in
let+ link = Database.Actor.of_remote user db in
let link = Result.get_ok link in
let@ remote_user_resolved = Database.Link.resolve link db in
check_is_ok remote_user_resolved
;;

T.add_test "can resolve remote users correctly" @@ with_db @@ fun db ->
let+ instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let instance = Result.get_ok instance in
let+ remote_user = Database.RemoteUser.create_remote_user
    ~username:"atestaccount"
    ~instance:(Database.RemoteInstance.self instance)
    ~url:"https://ocamlot.xyz/users/atestaccount"
    db in
let remote_user = Result.get_ok remote_user in
let+ link = Database.Actor.of_remote remote_user db in
let link = Result.get_ok link in
let* remote_user_resolved = Database.Link.resolve link db in
match remote_user_resolved with
| Database.Actor.Remote user -> check_string_eq ~expected:(Database.RemoteUser.username remote_user) (Database.RemoteUser.username user)
| Database.Actor.Local _ -> failwith "expected a local instance, retrieved a global one"
;;



T.add_test "can create local user" @@ with_db @@ fun db ->
let+ local_user = Database.LocalUser.create_user ~username:"example" ~password:"areallygoodpassword" db  in
let local_user = Result.get_ok local_user in
let@ actor = Database.Actor.of_local local_user db in
check_is_ok actor
;;

T.add_test "can create local user twice" @@ with_db @@ fun db ->
let+ local_user = Database.LocalUser.create_user ~username:"example" ~password:"areallygoodpassword" db  in
let local_user = Result.get_ok local_user in
let+ _ = Database.Actor.of_local local_user db in
let@ actor = Database.Actor.of_local local_user db in
check_is_ok actor
;;

T.add_test "can resolve local users" @@ with_db @@ fun db ->
let+ local_user = Database.LocalUser.create_user ~username:"example" ~password:"areallygoodpassword" db  in
let local_user = Result.get_ok local_user in
let+ link = Database.Actor.of_local local_user db in
let link = Result.get_ok link in
let@ local_user_resolved = Database.Link.resolve link db in
check_is_ok local_user_resolved
;;

T.add_test "can resolve local users correctly" @@ with_db @@ fun db ->
let+ local_user = Database.LocalUser.create_user ~username:"example" ~password:"areallygoodpassword" db  in
let local_user = Result.get_ok local_user in
let+ link = Database.Actor.of_local local_user db in
let link = Result.get_ok link in
let* local_user_resolved = Database.Link.resolve link db in
match local_user_resolved with
| Database.Actor.Local user -> check_string_eq ~expected:(Database.LocalUser.username local_user) (Database.LocalUser.username user)
| Database.Actor.Remote _ -> failwith "expected a local instance, retrieved a global one"
;;


let () =
  Mirage_crypto_rng_lwt.initialize ();
  T.run ()
