(lang dune 2.0)

(name activitypub-server)

(documentation "https://gopiandcode.github.io/activitypub-server/")

(source
 (github gopiandcode/activitypub-server))

(license MIT)

(authors
  "Kiran Gopinathan")

(maintainers
  "Kiran Gopinathan")

(generate_opam_files true)

(package
 (name activitypub-server)
 (synopsis "A simple single user activitypub federated server instance")
 (description "\
A simple single user activitypub federated server instance
")
 (depends
  (ocaml (>= 4.08.0))
  dune
  (sihl (>= 0.3.0))
  (tyxml-ppx (>= 4.4.0))
  (caqti-driver-postgresql
   (>= 1.2.1))
  (alcotest-lwt :with-test)
  (odoc :with-doc)))
