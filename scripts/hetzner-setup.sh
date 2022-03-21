# OCamlot setup instructions for Hetzner Hosting (debian in general)

# 1. download opam
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# 2. install general deps
sudo apt install libc-dev git make gcc argon2 unzip bubblewrap sqlite3 libev-dev libffi-dev libgmp-dev libsqlite3-dev libssl-dev pkg-config 

# 3. install sass
curl -L https://github.com/sass/dart-sass/releases/download/1.49.9/dart-sass-1.49.9-linux-x64.tar.gz -o /tmp/dart-sass.tar.gz
(cd /tmp/ && tar xzf ./dart-sass.tar.gz && sudo cp ./dart-sass/sass /usr/local/bin/)

# 4. initialise opam
opam init

# 5. add 4.12.0 switch
opam switch create 4.13.0

# 6. install opam deps
opam install dune \
     dream jwto\
     caqti caqti-driver-sqlite3\
     alcotest alcotest-lwt \
     yojson decoders decoders-yojson \
     ppx_deriving\
     argon2 sqlite3 x509 ptime bos tyxml \
     cohttp-lwt-unix \
     containers  \
     calendar caqti-type-calendar uuidm

# 7. download activitypub server
git clone https://gitlab.com/gopiandcode/activitypub-server.git
cd ./activitypub-server

# 8. setup sqlite
sqlite3 -init ./resources/schema.sql test.db
chmod u+rw ./_build/default/test.db

# 9. build project
dune build @all
