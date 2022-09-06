#!/bin/bash

cp /opt/prod.secret.exs /opt/pleroma/config/
cp /opt/prod.secret.exs /opt/pleroma/config/dev.secret.exs

echo "Installing HEX"
mix local.hex --force || exit
echo "Installing deps"
mix deps.get --force || exit
echo "Installing rebar"
mix local.rebar --force || exit

# migrate database
MIX_ENV=dev mix ecto.migrate

# update ca certificates
update-ca-certificates

# start pleroma
MIX_ENV=dev mix phx.server &

# start nginx
nginx -g "error_log /opt/pleroma/logs info;"

# read nginx logs
touch /opt/pleroma/logs
tail -f /opt/pleroma/logs
