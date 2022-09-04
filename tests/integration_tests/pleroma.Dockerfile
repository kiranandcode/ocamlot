FROM debian:11

# install deps
RUN apt update && apt install -y python3 curl unzip libncurses5 postgresql postgresql-contrib nginx certbot libmagic-dev imagemagick ffmpeg libimage-exiftool-perl

# install pleroma
RUN adduser --system --shell  /bin/false --home /opt/pleroma pleroma

USER pleroma

RUN curl 'https://git.pleroma.social/api/v4/projects/2/jobs/artifacts/stable/download?job=amd64' -o /tmp/pleroma.zip && \
    unzip /tmp/pleroma.zip -d /tmp/

RUN mv /tmp/release/* /opt/pleroma && \
    rmdir /tmp/release && \
    rm /tmp/pleroma.zip

USER root


RUN mkdir -p /var/lib/pleroma/uploads && \
    chown -R pleroma /var/lib/pleroma && \
    mkdir -p /var/lib/pleroma/static && \
    chown -R pleroma /var/lib/pleroma && \
    mkdir -p /etc/pleroma && \
    chown -R pleroma /etc/pleroma

USER pleroma

COPY ./tests/integration_tests/config.exs /etc/pleroma/config.exs

COPY ./tests/integration_tests/run_pleroma.sh /opt/pleroma/bin/run_pleroma.sh

USER root

# install https certificates
RUN ln -s /certs/ocamlot.crt /usr/local/share/ca-certificates/ocamlot.crt

# copy over nginx configuration
COPY ./tests/integration_tests/pleroma.nginx /etc/nginx/sites-enabled/pleroma.nginx

CMD ["/opt/pleroma/bin/run_pleroma.sh"]
