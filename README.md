# OCamlot - An OCaml Activitypub Server

OCamlot is an activitypub server written in OCaml!

As with most non-mainstream activitypub servers, it's currently in very experimental status!

Features:

 - Creating posts
 - Replying to posts
 - Custom profiles
 - Liking posts (we call them `toasts`)
 - Reboosting posts (we call them `cheers`)
 - Replying to posts

Screenshot:

![screenshot.png](https://raw.githubusercontent.com/Gopiandcode/ocamlot/master/screenshot.png)

You can see a running instance of the server at [ocamlot.xyz](https://ocamlot.xyz)!

## Deploying OCamlot

The rest of this guide will be assuming that you have a server with
OCaml installed, and nginx, setup with letsencrypt, such that an
appropriate internal port is accessible via a public domain.

Your nginx config may look something like as follows (using PORT
`7331` and DOMAIN `ocamlot.xyz`):
```
server {
    server_name ocamlot.xyz;

    listen 443 ssl; # managed by Certbot

    location / {
        proxy_pass http://localhost:7331;
    }
    ssl_certificate /etc/letsencrypt/live/ocamlot.xyz/fullchain.pem; # managed by Certbot
    ssl_certificate_key /etc/letsencrypt/live/ocamlot.xyz/privkey.pem; # managed by Certbot
    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot
}

server {
    if ($host = ocamlot.xyz) {
        return 301 https://$host$request_uri;
   } # managed by Certbot

   listen 80 default_server;
   listen [::]:80 default_server;

   return 404; # managed by Certbot
}

```

To deploy the server, do the following:

1. Create a new user to run the ocamlot process

```
$ sudo useradd -r -s /bin/false -m -d /var/lib/ocamlot -U ocamlot
```

2. Create a new postgres user and database:

```bash
$ sudo -Hu postgres psql

postgres=# CREATE ROLE ocamlot WITH password = '<password>';
CREATE ROLE

postgres=# ALTER ROLE ocamlot WITH login;
ALTER ROLE

postgres=# CREATE DATABASE ocamlot WITH OWNER ocamlot;
CREATE DATABASE
```

3. Clone the development repository:

```
$ sudo mkdir -p /opt/ocamlot
$ sudo chown -R ocamlot:ocamlot /opt/ocamlot
$ sudo -Hu ocamlot git clone https://codeberg.org/gopiandcode/ocamlot /opt/ocamlot
```

4. Switch user to the ocamlot user:

```
$ sudo -Hu ocamlot bash
```

5. Setup opam:

```
$ opam init

$ eval $(opam env)

$ opam update
```

6. Install dev dependencies:

```
$ cd /var/lib/ocamlot

$ git clone https://github.com/gopiandcode/petrol

$ cd ./petrol

$ opam pin .
```

7. Build the project

```
$ cd /opt/ocamlot

$ opam update && opam install --deps-only .

$ dune build
```

8. Modify `./scripts/run.sh` with domain and port:

```
# file: ./scripts/run.sh

#!/bin/bash
/opt/ocamlot/_build/default/bin/main.exe -u 'ocamlot:<password>@localhost:5432' -d "ocamlot.xyz" -p 7331 -D
```

9. Create an account via the web UI, with username `<username>`

10. Promote user `<username>` to admin

```
/opt/ocamlot/_build/default/bin/main.exe -u 'ocamlot:<password>@localhost:5432' -d "ocamlot.xyz" --promote-to-admin=<username>
```

11. Copy over `./scripts/ocamlot.service` to `etc/systemd/system/ocamlot.service`:

```
sudo cp /opt/ocamlot/scripts/ocamlot.service /etc/systemd/system/ocamlot.service
```
12. Enable and start `ocamlot.service`:

```
sudo systemctl enable --now ocamlot.service
```

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
git clone -b  v2.5.0 --single-branch https://git.pleroma.social/pleroma/pleroma ./tests/integration_tests/pleroma
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
