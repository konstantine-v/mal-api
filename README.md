# MAL-API

Unofficial MyAnimeList REST API (Jikan v4 compatible). Scrapes MAL and serves JSON at `/api`. Not affiliated with MyAnimeList or Jikan. This is a ocaml implementation of [jikan](https://github.com/jikan-me/jikan) by [jikan-me](https://github.com/jikan-me).

## Docker

```bash
cp .env.example .env
docker compose up --build
```

```bash
curl http://localhost:8080/api/anime/1
```

Data is stored in the `mal-data` volume (`CACHE_PATH=/data/mal.db`).

## Local

Needs OCaml 5.1+, opam, libsqlite3, libssl.

```bash
opam install . --deps-only -y
dune build
dune exec -- mal-api
```

Default bind is `0.0.0.0:8080`. Override with `PORT`, `BIND`, and `CACHE_PATH` (see `.env.example`).

## MCP

Streamable HTTP MCP is at `POST /mcp` (JSON-RPC). Point Cursor at:

```json
{
  "mcpServers": {
    "mal": {
      "url": "http://localhost:8080/mcp"
    }
  }
}
```

## Notes

Respect [MyAnimeList terms](https://myanimelist.net/about/terms_of_use). This service does not support authenticated list updates.
