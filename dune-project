(lang dune 3.16)

(name jikan-api)

(generate_opam_files true)

(license MIT)

(authors "jikan-api")

(maintainers "jikan-api")

(package
 (name jikan-api)
 (synopsis "Unofficial MyAnimeList REST API (Jikan v4 compatible)")
 (description
  "Self-hosted OCaml clone of Jikan REST v4. Scrapes MyAnimeList and serves the Jikan JSON contract.")
 (depends
  (ocaml
   (>= 5.1.0))
  dune
  dream
  lwt
  lwt_ppx
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  uri
  lambdasoup
  yojson
  caqti
  caqti-lwt
  caqti-driver-sqlite3
  logs
  fmt
  ptime
  re
  ssl))
