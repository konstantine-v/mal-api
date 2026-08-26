type t = int

let of_int n = if n > 0 then Some n else None

let of_string s =
  match int_of_string_opt (String.trim s) with
  | Some n -> of_int n
  | None -> None

let to_int t = t

let to_yojson t = `Int t

let pp fmt t = Format.fprintf fmt "%d" t
