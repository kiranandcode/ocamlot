# 1. Setup OPAM

# install to /home/private/bin
sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)

# add to path
export PATH="/home/private/bin:/home/private/.local/bin/:$PATH"

# need to install gpatch
# clone from here: https://mirror.netcologne.de/gnu/patch/
# extract file, cd into it, cun ./configure, then make to build the gpatch binary
# mv ./src/patch ../bin/gpatch

opam init

opam switch create 4.12.0

git clone https://gitlab.com/gopiandcode/activitypub-server.git

# install libev
# curl http://dist.schmorp.de/libev/libev-4.33.tar.gz -o libev-4.33.tar.gz
# ./configure
# make
# cp ./.libs/* ~/libs/

export LIBRARY_PATH="/home/private/libs/"
export LD_LIBRARY_PATH="/home/private/libs/"

export C_INCLUDE_PATH="/home/private/include/"

opam install caqti-driver-sqlite3 caqti jwto dream alcotest alcotest-lwt yojson ppx_yojson_conv ppx_deriving argon2 sqlite3 x509 ptime bos tyxml cohttp-lwt-unix containers brr iter

# install argon2
# curl https://codeload.github.com/P-H-C/phc-winner-argon2/tar.gz/20190702?dummy=/ -o argon2.tar.gz
# gmake
# cp ./*.so ~/libs/
