type t = {
  cfg : Config.t;
  cache : Cache.t;
  mal : Mal_client.t;
  heartbeat : Heartbeat.t;
}

let create cfg cache heartbeat =
  { cfg; cache; mal = Mal_client.create cfg heartbeat; heartbeat }

let fetch t ~url ~ttl ~parse ~encode =
  match%lwt Cache.get t.cache url with
  | Some "__404__" -> Lwt.return (Error Error.Not_found)
  | Some payload -> (
      try Lwt.return (Ok (Yojson.Safe.from_string payload))
      with _ -> Lwt.return (Error (Error.Internal "corrupt cache")))
  | None -> (
      match%lwt Mal_client.get t.mal url with
      | Error Error.Not_found ->
          let%lwt () =
            Cache.put t.cache url "__404__" t.cfg.cache_404_expire
          in
          Lwt.return (Error Error.Not_found)
      | Error e -> Lwt.return (Error e)
      | Ok html -> (
          match parse html with
          | Error e -> Lwt.return (Error e)
          | Ok domain ->
              let json = encode domain in
              let payload = Yojson.Safe.to_string json in
              let%lwt () = Cache.put t.cache url payload ttl in
              Lwt.return (Ok json)))
