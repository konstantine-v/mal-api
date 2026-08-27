let () =
  Ssl.init ();
  Random.self_init ();
  let cfg = Mal.Config.load () in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level cfg.log_level;
  let heartbeat = Mal.Heartbeat.create cfg in
  let cache = Lwt_main.run (Mal.Cache.create cfg.cache_path) in
  let app = Mal.App.create cfg cache heartbeat in
  Dream.run ~interface:cfg.bind ~port:cfg.port
  @@ Dream.logger
  @@ Mal.Routes.handler app
