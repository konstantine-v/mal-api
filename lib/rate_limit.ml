type t = { mutex : Lwt_mutex.t; mutable last : float; delay : float }

let create delay = { mutex = Lwt_mutex.create (); last = 0.; delay }

let wait t =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let now = Unix.gettimeofday () in
      let remaining = t.last +. t.delay -. now in
      let%lwt () =
        if remaining > 0. then Lwt_unix.sleep remaining else Lwt.return_unit
      in
      t.last <- Unix.gettimeofday ();
      Lwt.return_unit)
