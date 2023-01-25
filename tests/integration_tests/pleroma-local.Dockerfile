FROM elixir:1.14.3

VOLUME ["/opt/pleroma"]

# install deps
RUN apt update && apt install -y python3 curl sudo unzip git libncurses5 postgresql postgresql-contrib nginx certbot libmagic-dev imagemagick ffmpeg libimage-exiftool-perl build-essential cmake elixir erlang-dev erlang-nox 

# clone pleroma, install deps, and clean up
RUN git clone -b v2.5.0 https://git.pleroma.social/pleroma/pleroma /tmp/pleroma && \
    cd /tmp/pleroma && \
    mix local.hex --force && mix local.rebar --force && mix deps.get --force && \
    cd ../ && rm -r -f /tmp/pleroma


# copy over helper files
COPY ./tests/integration_tests/config.exs /opt/prod.secret.exs
COPY ./tests/integration_tests/run_pleroma_local.sh /opt/run_pleroma_local.sh

# configure environment
ENV MIX_ENV=prod
ENV HEX_UNSAFE_HTTPS=true

# install https certificates
RUN ln -s /certs/ocamlot.crt /usr/local/share/ca-certificates/ocamlot.crt

# copy over nginx configuration
COPY ./tests/integration_tests/pleroma.nginx /etc/nginx/sites-enabled/pleroma.nginx

# switch to working directory
WORKDIR /opt/pleroma

CMD ["/opt/run_pleroma_local.sh"]
