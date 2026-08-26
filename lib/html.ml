open Soup

let parse html = Soup.parse html

let trim s =
  let s = Str.global_replace (Str.regexp "[ \t\n\r]+") " " s in
  String.trim s

let texts_of node = trim (String.concat " " (texts node))

let attr_opt node name = attribute name node

let meta soup property =
  soup $? Printf.sprintf "meta[property=\"%s\"]" property
  |> fun n -> Option.bind n (fun el -> attr_opt el "content")

let spans soup =
  soup $$ "span" |> to_list

let span_with_text soup label =
  spans soup
  |> List.find_opt (fun n -> texts_of n = label)

let labeled soup label =
  match span_with_text soup label with
  | None -> None
  | Some span -> (
      match parent span with
      | None -> None
      | Some p ->
          let full = texts_of p in
          let prefix = label in
          let rest =
            if String.length full >= String.length prefix
               && String.sub full 0 (String.length prefix) = prefix
            then
              String.sub full (String.length prefix)
                (String.length full - String.length prefix)
            else full
          in
          let rest = trim rest in
          if rest = "" || rest = "?" || rest = "Unknown" || rest = "None"
             || rest = "N/A" || rest = "None found"
             || rest = "No genres have been added yet"
          then None
          else Some rest)

let labeled_int soup label =
  match labeled soup label with
  | None -> None
  | Some s ->
      let s = Str.global_replace (Str.regexp "[#,]") "" s in
      int_of_string_opt (trim s)

let labeled_float soup label =
  match labeled soup label with
  | None -> None
  | Some s -> float_of_string_opt (trim s)

let abs_url href =
  if String.length href >= 4 && String.sub href 0 4 = "http" then href
  else if String.length href > 0 && href.[0] = '/' then
    "https://myanimelist.net" ^ href
  else "https://myanimelist.net/" ^ href

let id_from_url url =
  let re =
    Re.compile
      (Re.seq
         [
           Re.str "myanimelist.net/";
           Re.rep1 (Re.alt [ Re.rg 'a' 'z'; Re.char '_' ]);
           Re.char '/';
           Re.group (Re.rep1 Re.digit);
         ])
  in
  match Re.exec_opt re url with
  | Some g -> Mal_id.of_string (Re.Group.get g 1)
  | None ->
      let re2 =
        Re.compile
          (Re.seq
             [
               Re.char '/';
               Re.rep1 (Re.alt [ Re.rg 'a' 'z'; Re.char '_' ]);
               Re.char '/';
               Re.group (Re.rep1 Re.digit);
             ])
      in
      Option.bind (Re.exec_opt re2 url) (fun g ->
          Mal_id.of_string (Re.Group.get g 1))

let resource_type url =
  let rec find = function
    | [] -> "anime"
    | part :: rest ->
        if
          List.mem part
            [
              "anime";
              "manga";
              "character";
              "people";
              "profile";
              "club";
              "forum";
              "news";
            ]
        then part
        else find rest
  in
  Uri.path (Uri.of_string url) |> String.split_on_char '/'
  |> List.filter (fun s -> s <> "")
  |> find

let mal_url_of_anchor a =
  match attr_opt a "href" with
  | None -> None
  | Some href ->
      let url = abs_url href in
      Option.map
        (fun mal_id ->
          ({
             mal_id;
             type_ = resource_type url;
             name = texts_of a;
             url;
           }
            : Types.mal_url))
        (id_from_url url)

let labeled_urls soup label =
  match span_with_text soup label with
  | None -> []
  | Some span -> (
      match parent span with
      | None -> []
      | Some p ->
          if
            let t = texts_of p in
            Re.execp
              (Re.compile
                 (Re.alt
                    [
                      Re.str "None found";
                      Re.str "No genres have been added yet";
                      Re.str "None";
                    ]))
              t
          then []
          else p $$ "a" |> to_list |> List.filter_map mal_url_of_anchor)

let labeled_urls_any soup labels =
  let rec go = function
    | [] -> []
    | label :: rest -> (
        match labeled_urls soup label with [] -> go rest | xs -> xs)
  in
  go labels

let images_from_url url =
  let jpg_url = url in
  let stem =
    if Filename.check_suffix url ".jpg" then Filename.chop_suffix url ".jpg"
    else if Filename.check_suffix url ".webp" then Filename.chop_suffix url ".webp"
    else if Filename.check_suffix url ".png" then Filename.chop_suffix url ".png"
    else url
  in
  let jpg : Types.images =
    {
      image_url = Some jpg_url;
      small_image_url = Some (stem ^ "t.jpg");
      large_image_url = Some (stem ^ "l.jpg");
    }
  in
  let webp : Types.images =
    {
      image_url = Some (stem ^ ".webp");
      small_image_url = Some (stem ^ "t.webp");
      large_image_url = Some (stem ^ "l.webp");
    }
  in
  ({ jpg; webp } : Types.image_pair)

let og_images soup =
  match meta soup "og:image" with
  | None -> Types.empty_image_pair
  | Some url -> images_from_url url

let months =
  [
    ("jan", 1);
    ("feb", 2);
    ("mar", 3);
    ("apr", 4);
    ("may", 5);
    ("jun", 6);
    ("jul", 7);
    ("aug", 8);
    ("sep", 9);
    ("oct", 10);
    ("nov", 11);
    ("dec", 12);
  ]

let parse_month_token tok =
  let tok = String.lowercase_ascii (String.sub tok 0 (min 3 (String.length tok))) in
  List.assoc_opt tok months

let parse_one_date s =
  let s = trim s in
  if s = "" || s = "?" || s = "Not available" then Types.empty_date_prop
  else
    let parts = String.split_on_char ' ' s |> List.filter (fun p -> p <> "") in
    match parts with
    | [ mon; day; year ] ->
        let day = int_of_string_opt (String.trim (String.map (function ',' -> ' ' | c -> c) day)) in
        {
          Types.day;
          month = parse_month_token mon;
          year = int_of_string_opt year;
        }
    | [ mon; year ] ->
        {
          day = None;
          month = parse_month_token mon;
          year = int_of_string_opt year;
        }
    | [ year ] ->
        { day = None; month = None; year = int_of_string_opt year }
    | _ -> Types.empty_date_prop

let iso_of (p : Types.date_prop) =
  match (p.year, p.month, p.day) with
  | Some y, Some m, Some d ->
      Some (Printf.sprintf "%04d-%02d-%02dT00:00:00+00:00" y m d)
  | Some y, Some m, None ->
      Some (Printf.sprintf "%04d-%02d-01T00:00:00+00:00" y m)
  | Some y, None, None -> Some (Printf.sprintf "%04d-01-01T00:00:00+00:00" y)
  | _ -> None

let parse_date_range s =
  match s with
  | None -> Types.empty_date_range
  | Some raw ->
      let raw = trim raw in
      let parts = Str.split (Str.regexp " to ") raw in
      let from_s, to_s =
        match parts with
        | [ a; b ] -> (Some (trim a), Some (trim b))
        | [ a ] -> (Some (trim a), None)
        | _ -> (None, None)
      in
      let from_prop =
        match from_s with None -> Types.empty_date_prop | Some x -> parse_one_date x
      in
      let to_prop =
        match to_s with None -> Types.empty_date_prop | Some x -> parse_one_date x
      in
      {
        Types.from_iso = iso_of from_prop;
        to_iso = iso_of to_prop;
        from_prop;
        to_prop;
        string_ = Some raw;
      }

let parse_broadcast s =
  match s with
  | None -> Types.empty_broadcast
  | Some raw ->
      let raw = trim raw in
      if raw = "Unknown" || raw = "Not scheduled once per week" then
        { Types.empty_broadcast with string_ = Some raw }
      else
        let day, time =
          match Str.split (Str.regexp " at ") raw with
          | day :: rest ->
              let time =
                match rest with
                | [] -> None
                | t :: _ ->
                    let t = Str.global_replace (Str.regexp " *(JST).*") "" t in
                    Some (trim t)
              in
              (Some (trim day), time)
          | [] -> (None, None)
        in
        {
          day;
          time;
          timezone = (if time <> None then Some "Asia/Tokyo" else None);
          string_ = Some raw;
        }

let parse_season_year s =
  match s with
  | None -> (None, None)
  | Some raw -> (
      match String.split_on_char ' ' (trim raw) with
      | [ season; year ] ->
          ( Some (String.lowercase_ascii season),
            int_of_string_opt year )
      | _ -> (None, None))

let youtube_from_href href =
  let re =
    Re.compile
      (Re.seq
         [
           Re.alt [ Re.str "youtu.be/"; Re.str "v="; Re.str "embed/" ];
           Re.group
             (Re.repn
                (Re.alt
                   [
                     Re.rg 'a' 'z';
                     Re.rg 'A' 'Z';
                     Re.rg '0' '9';
                     Re.char '-';
                     Re.char '_';
                   ])
                6 (Some 20));
         ])
  in
  match Re.exec_opt re href with
  | Some g ->
      let id = Re.Group.get g 1 in
      let url = "https://www.youtube.com/watch?v=" ^ id in
      let embed =
        "https://www.youtube-nocookie.com/embed/" ^ id
        ^ "?enablejsapi=1&wmode=opaque&autoplay=1"
      in
      ({
         Types.youtube_id = Some id;
         url = Some url;
         embed_url = Some embed;
         images =
           {
             image_url = Some ("https://img.youtube.com/vi/" ^ id ^ "/default.jpg");
             small_image_url =
               Some ("https://img.youtube.com/vi/" ^ id ^ "/sddefault.jpg");
             medium_image_url =
               Some ("https://img.youtube.com/vi/" ^ id ^ "/mqdefault.jpg");
             large_image_url =
               Some ("https://img.youtube.com/vi/" ^ id ^ "/hqdefault.jpg");
             maximum_image_url =
               Some ("https://img.youtube.com/vi/" ^ id ^ "/maxresdefault.jpg");
           };
       }
        : Types.trailer)
  | None -> Types.empty_trailer

let select_list soup selector = soup $$ selector |> to_list

let first_text soup selector =
  soup $? selector |> Option.map texts_of

let pagination_from_html soup ~page ~per_page ~count =
  let last =
    soup $$ ".pagination a, .pagination span, .link-hover-underline"
    |> to_list
    |> List.filter_map (fun n -> int_of_string_opt (texts_of n))
    |> List.fold_left max page
  in
  let has_next =
    match soup $? "a.next, a.link-hover-underline.next" with
    | Some _ -> true
    | None -> last > page
  in
  ({
     last_visible_page = max last 1;
     has_next_page = has_next;
     current_page = page;
     items =
       Some
         {
           count;
           total = last * per_page;
           per_page;
         };
   }
    : Json.pagination)
