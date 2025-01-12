FROM haskell:9.6

ARG APP_DIR="/opt/app"

RUN apt update
RUN apt install -y procps wget lsb-release libpq-dev

# install watchexec, postgresql-client-14
RUN curl -fsSL https://apt.cli.rs/pubkey.asc | tee -a /usr/share/keyrings/rust-tools.asc \
	&& curl -fsSL https://apt.cli.rs/rust-tools.list | tee /etc/apt/sources.list.d/rust-tools.list \
	&& sh -c 'echo "deb https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list' \
	&& wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
	&& apt update \
	&& apt install -y watchexec-cli postgresql-client-14

# install ghcup
RUN mkdir -p "${APP_DIR}/.ghcup/bin"
RUN curl -LJ "https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup" -o "${APP_DIR}/.ghcup/bin/ghcup"
RUN chmod +x "${APP_DIR}/.ghcup/bin/ghcup"

ENV PATH="${APP_DIR}/.cabal/bin:${APP_DIR}/.ghcup/bin:$PATH"

RUN ghcup install cabal recommended --set
RUN ghcup install hls recommended --set
RUN cabal update
RUN cabal install cabal-fmt implicit-hie

WORKDIR ${APP_DIR}/src
COPY ./src/ .

RUN cabal build

ENTRYPOINT ["watchexec", "-r", "-e", "hs", "--", "cabal", "run"]

