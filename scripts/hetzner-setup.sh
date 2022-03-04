# OCamlot setup instructions for Hetzner Hosting (debian in general)

# 1. download opam
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# 2. install general deps
sudo apt install libc-dev git make gcc argon2 unzip bubblewrap sqlite3 libev-dev libffi-dev libgmp-dev libsqlite3-dev libssl-dev pkg-config

# 3. install sass
curl -L https://github.com/sass/dart-sass/releases/download/1.49.9/dart-sass-1.49.9-linux-x64.tar.gz ../dart-sass.tar.gz
tar xzf ../dart-sass.tar.gz
sudo cp ./dart-sass/sass /usr/local/bin/
rm -fr ./dart-sass # fr fr no cap


# 4. initialise opam
opam init

# 5. add 4.12.0 switch
opam switch create 4.12.0

# 6. load activitypub server
git clone https://gitlab.com/gopiandcode/activitypub-server.git

# 7. 
cd ./activitypub-server

# install deps
opam install dune caqti-driver-sqlite3 caqti jwto dream alcotest alcotest-lwt yojson ppx_yojson_conv ppx_deriving argon2 sqlite3 x509 ptime bos tyxml cohttp-lwt-unix containers brr iter
eval $(opam env)

