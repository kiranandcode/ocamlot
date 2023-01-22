# OCamlot - An OCaml Activitypub Server


## Development Setup

To develop OCamlot locally, we provide a docker-compose file for
setting up a network with a running OCamlot (port 7331) and pleroma
(port 4000) instance. The two containers are connected through a
network in which the OCamlot server can be found under the domain name
`testing.ocamlot.xyz` and the Pleroma server can be found under the
domain name `pleroma.ocamlot.xyz`. (Note: because there is no easy way
of updating the CA store that elixir uses, you will need to patch your
local copy of pleroma to disable certification verification for
interaction between the two containers to work).

*You will need docker-compose and docker*

1. Clone a copy of pleroma to the `tests/integration_test/` directory:

```bash
git clone -b  v2.4.5 --single-branch https://git.pleroma.social/pleroma/pleroma ./tests/integration_tests/pleroma
```

2. Apply patch to pleroma to disable TLS validation (required for local testing)

```bash
(cd ./tests/integration_tests/pleroma && git apply ../pleroma.patch)
```

3. Change directory into the `tests/integration_test/` directory:

```bash
cd ./tests/integration_tests/
```

4. Run `docker-compose build` with the local docker-compose file:

```bash
docker-compose -f ./docker-compose.local.yml build
```

5. Run `docker-compose up` with the local docker-compose file:

```bash
docker-compose -f ./docker-compose.local.yml up
```
Now pleroma will be available at `https://localhost:4000` and ocamlot at `https://localhost:7331`

6. (optional), if you want to update the OCamlot server after making some changes locally, do the following:

   - 6.1. Find out the name of the OCamlot server container using `docker container list`
   - 6.2. Attach to the OCamlot container using `docker container exec -it <container-id> bash`
   - 6.3. Kill the running OCamlot process using `pkill -9 main.exe`
   - 6.4. Run `eval $(opam env)`, then `dune build` then `dune exec -- ./bin/main.exe -d testing.ocamlot.xyz -D`
