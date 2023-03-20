FROM ruby:3.2.1-bullseye

VOLUME ["/opt/mastadon"]

# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install -y --no-install-recommends build-essential \
        ca-certificates git libicu-dev libidn11-dev libpq-dev \
        libjemalloc-dev zlib1g-dev libgdbm-dev libgmp-dev \
        libssl-dev libyaml-0-2 ca-certificates libreadline8 \
        python3 shared-mime-info whois wget procps libssl1.1 \
        libpq5 imagemagick ffmpeg libjemalloc2 libicu67 libidn11 \
        libyaml-0-2 file ca-certificates tzdata libreadline8 tini 

RUN curl -sL https://deb.nodesource.com/setup_16.x -o /tmp/nodesource_setup.sh && \
    bash /tmp/nodesource_setup.sh && \
    curl -sL https://dl.yarnpkg.com/debian/pubkey.gpg | gpg --dearmor | tee /usr/share/keyrings/yarnkey.gpg >/dev/null && \
    echo "deb [signed-by=/usr/share/keyrings/yarnkey.gpg] https://dl.yarnpkg.com/debian stable main" | tee /etc/apt/sources.list.d/yarn.list && \
    apt update && apt-get install -y nodejs yarn

# clone mastadon, install deps, and clean up
RUN git clone https://github.com/mastodon/mastodon.git /tmp/mastadon && \
    cd /tmp/mastadon && \
    bundle config set --local deployment 'true' && \
    bundle config set silence_root_warning true && \
    bundle install -j"$(nproc)" && \
    yarn install --pure-lockfile --network-timeout 600000

ENV RAILS_ENV="production" \
    NODE_ENV="production" \
    RAILS_SERVE_STATIC_FILES="true" \
    BIND="0.0.0.0"

ENV OPT_SECRET="lol" SECRET_KEY_BASE="afsadkfjflskdafjiowewe"

COPY ./tests/integration_tests/.env.production /opt/.env.production
COPY ./tests/integration_tests/run_mastodon.sh /opt/run_mastodon.sh

# install https certificates
RUN ln -s /certs/ocamlot.crt /usr/local/share/ca-certificates/ocamlot.crt

# copy over nginx configuration
COPY ./tests/integration_tests/mastodon.nginx /etc/nginx/sites-enabled/mastodon.nginx


WORKDIR /opt/mastodon
CMD ["/opt/run_mastodon.sh"]

# # Precompile assets
# RUN OTP_SECRET=precompile_placeholder SECRET_KEY_BASE=precompile_placeholder rails assets:precompile

# # Set the work dir and the container entry point
# ENTRYPOINT ["/usr/bin/tini", "--"]
# EXPOSE 3000 4000
