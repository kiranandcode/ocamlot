From ocaml/opam:debian-11-ocaml-4.14

# set workdir
WORKDIR /home/opam/code

# install deps
RUN sudo apt install -y nginx libev-dev libgmp-dev pkg-config libffi-dev libsqlite3-dev libssl-dev libargon2-1

# copy the opam file only first
COPY --chown=opam ./ocamlot.opam .

# install files
RUN opam install --deps-only . -y

# copy over runner script
COPY --chown=opam ./tests/integration_tests/run_ocamlot.sh .

VOLUME ["/certs"]

# copy over files, manually
COPY --chown=opam ./bin ./bin
COPY --chown=opam ./lib ./lib
COPY --chown=opam ./macros ./macros
COPY --chown=opam ./resources ./resources
COPY --chown=opam ./static ./static
COPY --chown=opam ./dune-project ./dune-project

# build project
RUN sudo chown -R opam:opam ./ && eval $(opam env) && dune build @all

# default port that OCamlot listens on
EXPOSE 7331

# install https certificates
RUN sudo ln -s /certs/ocamlot.crt /usr/local/share/ca-certificates/ocamlot.crt

# copy over nginx configuration
COPY ./tests/integration_tests/ocamlot.nginx /etc/nginx/sites-enabled/ocamlot.nginx

# run OCamlot with default debug settings
ENTRYPOINT ["./run_ocamlot.sh"]
