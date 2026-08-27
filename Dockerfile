FROM ocaml/opam:debian-12-ocaml-5.2 AS build

USER root
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libsqlite3-dev \
    libssl-dev \
    libgmp-dev \
    pkg-config \
  && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/app
COPY --chown=opam:opam dune-project mal-api.opam* ./
COPY --chown=opam:opam dune ./
COPY --chown=opam:opam lib lib
COPY --chown=opam:opam bin bin

RUN opam install . --deps-only --yes \
  && eval "$(opam env)" \
  && dune build --release bin/mal_api.exe

FROM debian:bookworm-slim

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libsqlite3-0 \
    libssl3 \
    libgmp10 \
    ca-certificates \
  && rm -rf /var/lib/apt/lists/* \
  && mkdir -p /data

COPY --from=build /home/opam/app/_build/default/bin/mal_api.exe /usr/local/bin/mal-api

ENV PORT=8080 \
    BIND=0.0.0.0 \
    CACHE_PATH=/data/mal.db

EXPOSE 8080
VOLUME ["/data"]

CMD ["mal-api"]
