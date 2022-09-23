#!/bin/bash

# This is a helper script for testing that first installs
# ca-certificates at the root, then runs OCamlot

# update opam env
eval $(opam env)

# make certs accessible to opam
sudo chown -R opam:opam /certs

# update certificates
sudo update-ca-certificates

# start ocamlot
dune exec ./bin/main.exe -- -d "testing.ocamlot.xyz" -D &

# start nginx
sudo nginx -g "error_log /opt/logs info;"

# read nginx logs
sudo touch /opt/logs
sudo tail -f /opt/logs
