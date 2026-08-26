type t = {
  get : string -> string option Lwt.t;
  put : string -> string -> int -> unit Lwt.t;
}

let schema =
  {|CREATE TABLE IF NOT EXISTS cache (
      key TEXT PRIMARY KEY,
      payload TEXT NOT NULL,
      expires_at INTEGER NOT NULL
    )|}

let connect_uri path =
  let dir = Filename.dirname path in
  (if dir <> "." && dir <> "" && not (Sys.file_exists dir) then
     try Unix.mkdir dir 0o755
     with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Printf.sprintf "sqlite3:%s?create=true&busy_timeout=5000" path

let now () = int_of_float (Unix.gettimeofday ())

let create path =
  let uri = Uri.of_string (connect_uri path) in
  let pool =
    match Caqti_lwt_unix.connect_pool uri with
    | Error err -> failwith (Caqti_error.show err)
    | Ok pool -> pool
  in
  let open Caqti_request.Infix in
  let q_schema = (Caqti_type.unit ->. Caqti_type.unit) schema in
  let%lwt result =
    Caqti_lwt_unix.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec q_schema ())
      pool
  in
  (match result with
  | Error err -> failwith (Caqti_error.show err)
  | Ok () -> ());
  let get key =
    let q =
      (Caqti_type.(t2 string int) ->? Caqti_type.string)
        "SELECT payload FROM cache WHERE key = ? AND expires_at > ?"
    in
    let%lwt result =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find_opt q (key, now ()))
        pool
    in
    match result with
    | Error err ->
        Logs.warn (fun m -> m "cache get: %s" (Caqti_error.show err));
        Lwt.return None
    | Ok v -> Lwt.return v
  in
  let put key payload ttl =
    let q =
      (Caqti_type.(t3 string string int) ->. Caqti_type.unit)
        "INSERT OR REPLACE INTO cache (key, payload, expires_at) VALUES (?, ?, ?)"
    in
    let expires = now () + ttl in
    let%lwt result =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          Db.exec q (key, payload, expires))
        pool
    in
    match result with
    | Error err ->
        Logs.warn (fun m -> m "cache put: %s" (Caqti_error.show err));
        Lwt.return_unit
    | Ok () -> Lwt.return_unit
  in
  Lwt.return { get; put }

let get t key = t.get key

let put t key payload ttl = t.put key payload ttl
