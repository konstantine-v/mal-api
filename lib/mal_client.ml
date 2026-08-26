type t = {
  cfg : Config.t;
  limiter : Rate_limit.t;
  heartbeat : Heartbeat.t;
}

let create cfg heartbeat =
  {
    cfg;
    limiter = Rate_limit.create cfg.Config.source_delay;
    heartbeat;
  }

let is_mal_404 body =
  let lower = String.lowercase_ascii body in
  Re.execp
    (Re.compile
       (Re.alt
          [
            Re.str "404 not found";
            Re.str "error 404";
            Re.str "page not found";
            Re.str "this page doesn't exist";
          ]))
    lower

let get t url =
  Heartbeat.maybe_clear_failover t.heartbeat;
  let%lwt () = Rate_limit.wait t.limiter in
  let uri = Uri.of_string url in
  let headers =
    Cohttp.Header.of_list
      [
        ("User-Agent", t.cfg.mal_user_agent);
        ("Accept", "text/html,application/xhtml+xml");
        ("Accept-Language", "en-US,en;q=0.9");
      ]
  in
  Lwt.catch
    (fun () ->
      let%lwt resp, body =
        Lwt_unix.with_timeout t.cfg.source_timeout (fun () ->
            Cohttp_lwt_unix.Client.get ~headers uri)
      in
      let%lwt body = Cohttp_lwt.Body.to_string body in
      let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
      match code with
      | 404 ->
          let%lwt () = Heartbeat.record t.heartbeat ~ok:true in
          Lwt.return (Error Error.Not_found)
      | 429 ->
          let%lwt () = Heartbeat.record t.heartbeat ~ok:false in
          Lwt.return (Error Error.Rate_limited)
      | n when n >= 500 ->
          let%lwt () = Heartbeat.record t.heartbeat ~ok:false in
          Lwt.return (Error Error.Mal_unavailable)
      | n when n >= 200 && n < 300 ->
          if is_mal_404 body then (
            let%lwt () = Heartbeat.record t.heartbeat ~ok:true in
            Lwt.return (Error Error.Not_found))
          else (
            let%lwt () = Heartbeat.record t.heartbeat ~ok:true in
            Lwt.return (Ok body))
      | _ ->
          let%lwt () = Heartbeat.record t.heartbeat ~ok:false in
          Lwt.return (Error Error.Mal_unavailable))
    (function
      | Lwt_unix.Timeout ->
          let%lwt () = Heartbeat.record t.heartbeat ~ok:false in
          Lwt.return (Error Error.Mal_unavailable)
      | exn ->
          Logs.err (fun m -> m "mal fetch: %s" (Printexc.to_string exn));
          let%lwt () = Heartbeat.record t.heartbeat ~ok:false in
          Lwt.return (Error Error.Mal_unavailable))
