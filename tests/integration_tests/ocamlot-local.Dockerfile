From ocaml/opam:debian-11-ocaml-4.14

# install deps
RUN sudo apt update && sudo apt install -y nginx libev-dev libgmp-dev pkg-config libffi-dev libsqlite3-dev libssl-dev libargon2-1

USER root

VOLUME ["/certs"]
VOLUME ["/home/opam/code"]

WORKDIR /home/opam

# update files
RUN opam update

# install petrol and pin dev release
RUN git clone https://github.com/gopiandcode/petrol && (cd ./petrol && opam pin . )
RUN git clone https://github.com/ocaml/omd.git && (cd omd && opam pin .)

# set temp workdir
WORKDIR /tmp/code

# copy the opam file only first
COPY ./ocamlot.opam .

# install files
RUN opam install --deps-only . -y

# default port that OCamlot listens on
EXPOSE 7331

# install https certificates
RUN sudo ln -s /certs/ocamlot.crt /usr/local/share/ca-certificates/ocamlot.crt

# copy over nginx configuration
COPY ./tests/integration_tests/ocamlot.nginx /etc/nginx/sites-enabled/ocamlot.nginx

# copy over the runner script
COPY ./tests/integration_tests/run_ocamlot_local.sh /home/opam/run_ocamlot_local.sh
RUN chmod u+x /home/opam/run_ocamlot_local.sh

# setup work dir
WORKDIR /home/opam/code

# run OCamlot with default debug settings
ENTRYPOINT ["/home/opam/run_ocamlot_local.sh"]
