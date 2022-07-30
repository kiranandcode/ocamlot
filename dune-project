(lang dune 2.9)

(generate_opam_files true)

(license AGPL-3.0-or-later)

(package (name ocamelot)
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

   jwt 

   angstrom
   re
) )
