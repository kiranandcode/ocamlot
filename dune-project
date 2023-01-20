(lang dune 2.9)
(cram enable)
(generate_opam_files true)

(license AGPL-3.0-or-later)
(maintainers "The OCamlot developers")

(package (name ocamlot)
 (authors "The OCamlot developers")
 (homepage "https://ocamlot.xyz")
 (bug_reports "https://ocamlot.xyz/bug-reports")
 (synopsis "A lightweight activitypub server in OCaml")
 (description "OCamlot is a small OCaml library that provides a robust and efficient activitypub server.")
 (depends
   alcotest alcotest-lwt

   containers
   ptime

   mirage-crypto-rng
   argon2

   cohttp cohttp-lwt cohttp-lwt-unix

   caqti
   caqti-driver-sqlite3
   caqti-type-calendar

   uuidm

   dream
   tyxml
   uri

   decoders
   decoders-yojson
   yojson

   ppxlib
   ppx_deriving

   jwto

   angstrom
   re

   omd crunch ppx_blob
   
) )
