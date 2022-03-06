#!/bin/bash

dune build @all > /dev/null 2>&1

chmod u+rw ./_build/default/test.db

cd _build/default || exit

./lib/server/server.exe  &

BG_PID="$!"

cd - || exit

inotifywait -m . -r --exclude "_build/*" |
    while read -r directory events filename; do

        if [[ ! "$events" =~ .*(MODIFY|CLOSE_NOWRITE|OPEN|ISDIR|ACCESS).* ]]; then


            if [[ ! "$filename" =~ ^\.#  ]] && [[ ! "$filename" = "test.db" ]] && [[ ! "$filename" = "test.db-journal" ]] && [[ ! "$directory" = "./.git/" ]] && [[ ! "$filename" =~ ^# ]]; then
                echo "$directory" "$events" "$filename"


                if [ -n "$BG_PID" ]; then
                    echo "KILL $BG_PID"
                    kill -9 "$BG_PID"
                    sleep 4

                    dune build @all > /dev/null 2>&1

                    chmod u+rw ./_build/default/test.db

                    cd _build/default || exit

                    ./lib/server/server.exe &

                    BG_PID="$!"

                    cd - || exit

                else
                    echo "NOT KILL $BG_PID"
                fi

            fi
        fi

    done
