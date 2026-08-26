# jikan-api

Unofficial MyAnimeList REST API (Jikan v4 compatible). Scrapes MAL and serves JSON at `/v4`. Not affiliated with MyAnimeList.

## Docker

```bash
cp .env.example .env
docker compose up --build
```

```bash
curl http://localhost:8080/v4/anime/1
```

Data is stored in the `jikan-data` volume (`CACHE_PATH=/data/jikan.db`).

## Local

Needs OCaml 5.1+, opam, libsqlite3, libssl.

```bash
opam install . --deps-only -y
dune build
dune exec -- jikan-api
```

Default bind is `0.0.0.0:8080`. Override with `PORT`, `BIND`, and `CACHE_PATH` (see `.env.example`).

## Notes

Respect [MyAnimeList terms](https://myanimelist.net/about/terms_of_use). This service does not support authenticated list updates.
