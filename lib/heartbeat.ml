type t = {
  mutex : Lwt_mutex.t;
  mutable window : (float * bool) list;
  mutable last_downtime : float option;
  mutable failover : bool;
  mutable last_recheck : float;
  threshold : int;
  range : float;
  recheck : float;
  good_score : float;
}

let create (cfg : Config.t) =
  {
    mutex = Lwt_mutex.create ();
    window = [];
    last_downtime = None;
    failover = false;
    last_recheck = 0.;
    threshold = cfg.source_bad_health_threshold;
    range = cfg.source_bad_health_range;
    recheck = cfg.source_bad_health_recheck;
    good_score = cfg.source_good_health_score;
  }

let prune now range window =
  List.filter (fun (t, _) -> now -. t <= range) window

let score window =
  match window with
  | [] -> 1.0
  | _ ->
      let ok = List.fold_left (fun acc (_, s) -> if s then acc + 1 else acc) 0 window in
      float_of_int ok /. float_of_int (List.length window)

let record t ~ok =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let now = Unix.gettimeofday () in
      t.window <- prune now t.range ((now, ok) :: t.window);
      if not ok then t.last_downtime <- Some now;
      let fails =
        List.fold_left (fun acc (_, s) -> if s then acc else acc + 1) 0 t.window
      in
      if fails >= t.threshold then t.failover <- true
      else if score t.window >= t.good_score then t.failover <- false;
      Lwt.return_unit)

let maybe_clear_failover t =
  let now = Unix.gettimeofday () in
  if t.failover && now -. t.last_recheck >= t.recheck then (
    t.last_recheck <- now;
    t.failover <- false)

let status t = if t.failover then "offline" else "healthy"

let score_value t = score t.window

let is_down t = t.failover

let last_downtime t =
  match t.last_downtime with
  | None -> None
  | Some ts ->
      let tm = Unix.gmtime ts in
      Some
        (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d+00:00"
           (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
           tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)
