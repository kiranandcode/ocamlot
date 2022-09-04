#!/bin/bash

# migrate database
/opt/pleroma/bin/pleroma_ctl migrate

# update ca certificates
update-ca-certificates

# start pleroma
/opt/pleroma/bin/pleroma start &

# start nginx
nginx -g "error_log /opt/pleroma/logs info;"

# read nginx logs
touch /opt/pleroma/logs
tail -f /opt/pleroma/logs
