[@@@warning "-33"]
open Testing_utils.Common
module T = Testing_utils.Lwt.Make (struct let name = "instance" end);;

T.add_test "can create instance" @@ with_db @@ fun db ->
let+ user = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
ret begin check_is_ok user end
;;

T.add_test "instance url matches" @@ with_db @@ fun db ->
let* instance = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
ret begin check_string_eq ~expected:"ocamlot.xyz" instance.url end
;;

T.add_test "can lookup instances" @@ with_db @@ fun db ->
let+ _ = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let+ _ = Database.RemoteInstance.create_instance "ocamlot.nfshost.com" db in
let* result = Database.RemoteInstance.lookup_instance "ocamlot.xyz" db in
ret begin
  check_is_some result
end
;;

T.add_test "create duplicates returns original" @@ with_db @@ fun db ->
let+ ocamlot = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
let ocamlot = Result.get_ok ocamlot in
let* ocamlot' = Database.RemoteInstance.create_instance "ocamlot.xyz" db in
ret begin
  check_string_eq ~expected:ocamlot.url ocamlot'.url
end
;;


let () =
  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna);
  T.run ()
