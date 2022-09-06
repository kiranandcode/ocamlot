FROM debian:11

# install deps
RUN apt update && apt install -y python3 curl sudo unzip git libncurses5 postgresql postgresql-contrib nginx certbot libmagic-dev imagemagick ffmpeg libimage-exiftool-perl build-essential cmake elixir erlang-dev erlang-nox 

# install pleroma
RUN sudo useradd -r -s /bin/false -m -d /var/lib/pleroma -U pleroma

RUN mkdir -p /opt/pleroma

RUN sudo chown -R pleroma:pleroma /opt/pleroma

RUN sudo -Hu pleroma git clone -b stable https://git.pleroma.social/pleroma/pleroma /opt/pleroma

WORKDIR /opt/pleroma

USER root

RUN mkdir -p /var/lib/pleroma/uploads && \
    chown -R pleroma /var/lib/pleroma && \
    mkdir -p /var/lib/pleroma/static && \
    chown -R pleroma /var/lib/pleroma && \
    mkdir -p /etc/pleroma && \
    chown -R pleroma /etc/pleroma

USER pleroma

RUN mix local.hex --force && mix local.rebar --force && mix deps.get --force

COPY ./tests/integration_tests/config.exs /opt/pleroma/config/prod.secret.exs

COPY ./tests/integration_tests/run_pleroma.sh /opt/pleroma/bin/run_pleroma.sh

ENV MIX_ENV=prod
ENV HEX_UNSAFE_HTTPS=true

# hacks
RUN sed -i -e '28i Keyword.put(opts, :ssl_options, versions: [:"tlsv1.2", :"tlsv1.1", :tlsv1], cacertfile: "/certs/ocamlot.crt", verify: :verify_none)' -e '28d' ./lib/pleroma/http/adapter_helper/hackney.ex
RUN sed -i -e '10i opts = Keyword.put(opts, :ssl_options, versions: [:"tlsv1.2", :"tlsv1.1", :tlsv1], cacertfile: "/certs/ocamlot.crt", verify: :verify_none)' -e '10d' lib/pleroma/reverse_proxy/client/hackney.ex
RUN sed -i -e '27 i verify: :verify_none,' -e '27 i log_level: :warning' -e '27,32d' ./lib/pleroma/gun/conn.ex
RUN sed -i -e '40iKeyword.put(opts, :certificates_verification, false)'  -e '40d' ./lib/pleroma/http/adapter_helper/gun.ex
RUN sed -i -e '27i adapter_opts = Keyword.merge(opts[:adapter], conn: conn_pid, close_conn: false, certificates_verification: false)' -e '27d' ./lib/pleroma/tesla/middleware/connection_pool.ex
RUN sed -i -e '34i Logger.warn("==========GGGGGGGGGGGg adding tls opts #{inspect(tls_opts)}===============")' ./lib/pleroma/gun/conn.ex
RUN sed -i -e '215i Logger.debug(fn -> "Couldnt finger #{account} #{inspect(response)}" end)' -e '211i Logger.debug(fn -> "Couldnt finger #{account} because invalid content_type: #{inspect(content_type)}, resp: #{inspect(response)}" end)' lib/pleroma/web/web_finger.ex
# compile server
RUN mix phx.server --fail || echo

USER root

# install https certificates
RUN ln -s /certs/ocamlot.crt /usr/local/share/ca-certificates/ocamlot.crt

# copy over nginx configuration
COPY ./tests/integration_tests/pleroma.nginx /etc/nginx/sites-enabled/pleroma.nginx

CMD ["/opt/pleroma/bin/run_pleroma.sh"]
