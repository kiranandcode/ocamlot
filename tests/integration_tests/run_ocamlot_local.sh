#!/bin/bash

# update opam env
eval $(opam env)

# make certs accessible to opam
sudo chown -R opam:opam /certs

# update certificates
sudo update-ca-certificates

# first build
dune clean && dune build @all

# start ocamlot
dune exec ./bin/main.exe -- -u "ocamlot:password@postgres:5432" -d "testing.ocamlot.xyz" -D &

# start nginx
sudo nginx -g "error_log /opt/logs info;"

# read nginx logs
sudo touch /opt/logs
sudo tail -f /opt/logs
