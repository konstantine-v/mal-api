let () =
  Ssl.init ();
  Random.self_init ();
  let cfg = Jikan.Config.load () in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level cfg.log_level;
  let heartbeat = Jikan.Heartbeat.create cfg in
  let cache = Lwt_main.run (Jikan.Cache.create cfg.cache_path) in
  let app = Jikan.App.create cfg cache heartbeat in
  Dream.run ~interface:cfg.bind ~port:cfg.port
  @@ Dream.logger
  @@ Jikan.Routes.handler app
