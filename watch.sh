#!/bin/bash


build_project () {
    opam exec -- dune build @all

    if [[ ! "$?" -eq 0 ]]; then
        echo "failed to build project"
        exit -1
    fi
}
run_project () {
    opam exec -- dune exec -- ./bin/main.exe -D &
}

build_project

run_project
BG_PID="$!"

echo "$BG_PID"

inotifywait -m . -r -e "MODIFY" --exclude "(_build/*|.git/*|.*/\.#.*|\./.*\.db|\./.*\.db-wal|\./.*\.db-shm)" |
    while read -r directory events filename; do
        echo "received event $events on $directory:$filename, restarting"

        build_project

        opam exec -- dune build @all

        while (ps -p "$BG_PID" > /dev/null) && [[ ! -z "$BG_PID" ]]; do
            echo kill -INT $BG_PID
            kill -INT $BG_PID
        done
        run_project
        BG_PID="$!"
    done
