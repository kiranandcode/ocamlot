From ocaml/opam

WORKDIR /usr/src/app

Run sudo apt update && \
    sudo apt install -y postgresql libpq-dev libev-dev pkg-config libssl-dev

RUN opam update && \
    opam install -y dune containers dream caqti-driver-postgresql \
     caqti alcotest-lwt lwt odoc


COPY activitypub-server.opam \
    database lib test \
    dune dune-project \
    /usr/src/app/
