let null = `Null

let opt f = function None -> `Null | Some x -> f x

let opt_int (n : int option) = opt (fun n -> `Int n) n

let opt_float (n : float option) = opt (fun n -> `Float n) n

let opt_bool (n : bool option) = opt (fun n -> `Bool n) n

let opt_string (s : string option) = opt (fun s -> `String s) s

let string_list xs = `List (List.map (fun s -> `String s) xs)

let list f xs = `List (List.map f xs)

let data json = `Assoc [ ("data", json) ]

type items = { count : int; total : int; per_page : int }

let items_to_yojson (i : items) =
  `Assoc
    [
      ("count", `Int i.count);
      ("total", `Int i.total);
      ("per_page", `Int i.per_page);
    ]

type pagination = {
  last_visible_page : int;
  has_next_page : bool;
  current_page : int;
  items : items option;
}

let pagination_to_yojson (p : pagination) =
  `Assoc
    [
      ("last_visible_page", `Int p.last_visible_page);
      ("has_next_page", `Bool p.has_next_page);
      ("current_page", `Int p.current_page);
      ("items", opt items_to_yojson p.items);
    ]

let page ~pagination ~data =
  `Assoc [ ("pagination", pagination_to_yojson pagination); ("data", data) ]

let empty_pagination page =
  {
    last_visible_page = 1;
    has_next_page = false;
    current_page = page;
    items = Some { count = 0; total = 0; per_page = 25 };
  }
