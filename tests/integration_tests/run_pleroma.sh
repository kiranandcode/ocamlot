#!/bin/bash

# make certs file accessible to pleroma
chown pleroma:pleroma /certs/ocamlot.crt

# migrate database
sudo -Hu pleroma MIX_ENV=prod mix ecto.migrate

# update ca certificates
update-ca-certificates

# start pleroma
sudo -Hu pleroma MIX_ENV=prod mix phx.server &

# start nginx
nginx -g "error_log /opt/pleroma/logs info;"

# read nginx logs
touch /opt/pleroma/logs
tail -f /opt/pleroma/logs
