#!/bin/bash

# migrate database
su pleroma /opt/pleroma/bin/pleroma_ctl migrate

# update ca certificates
sudo update-ca-certificates

# start pleroma
/opt/pleroma/bin/pleroma start &

# start nginx
nginx -g "error_log /opt/pleroma/logs info;"

# read nginx logs
touch /opt/pleroma/logs
tail -f /opt/pleroma/logs
