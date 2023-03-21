#!/bin/bash

# 96c6671e85f0a2fb72572ff66c41218e
# ./bin/tootctl accounts create --email test@ocamlot.xyz --confirmed testuser
export RAILS_ENV='development'
export RAILS_LOG_LEVEL='debug'
export DB_NAME='mastodon_production'
export DB_HOST='postgres'
export DB_USER='mastodon'
export DB_PASS='password'
export REDIS_HOST='redis'
export LOCAL_DOMAIN=mastodon.ocamlot.xyz
export SINGLE_USER_MODE=false
export SECRET_KEY_BASE=a3ed02eb70537be5dcb1538369c96abed7a5134d4bf7383b431e3fa70de8850d9d9e17eb1df23039c8314a6c41a8ef0acf5e25a1aa38acb7352140da46370024
export OTP_SECRET=c85fb7149a5e3791ecdd7b33ce94789bb5deb77c230db658d8c8f34911babaf6fc8028268d0f78812002ea14294ee3c33ca9c77a26dfd7ee5da46c214a786fb8
export VAPID_PRIVATE_KEY=yq4eX9bO3ZUz6p69GntIrvtdNAhjTYMm8MRb8sq0l4o=
export VAPID_PUBLIC_KEY=BMt4Qa_6Q3mkfeoij5tM8OrW5XCX4SjdnGP4VaLX2PZ_KcQDH0Gcyl-ewBXGMRVIlW5mM4yx9zoHvtdD7fJOWeQ=
export DB_HOST=postgres
export DB_PORT=5432
export DB_NAME=mastodon
export DB_USER=mastodon
export DB_PASS=password
export REDIS_HOST=redis
export REDIS_PORT=6379
export REDIS_PASSWORD=
export SMTP_SERVER=smtp.mailgun.org
export SMTP_PORT=587
export SMTP_LOGIN=
export SMTP_PASSWORD=
export SMTP_AUTH_METHOD=plain
export SMTP_OPENSSL_VERIFY_MODE=none
export SMTP_ENABLE_STARTTLS=never
export SMTP_FROM_ADDRESS='Mastodon <notifications@mastodon.ocamlot.xyz>'

cp /opt/.env.production ./.env.production 
chown 1000:1000 ./.env.production

# setup db
bundle exec rake db:setup
bundle exec rake db:migrate

# precompile assets
bundle exec rake assets:precompile

# update ca certificates
update-ca-certificates

# start nginx
nginx -g "error_log /opt/logs info;"

# run mastadong
PORT=3000 bundle exec puma -C config/puma.rb &

# read nginx logs
touch /opt/logs
tail -f /opt/logs

