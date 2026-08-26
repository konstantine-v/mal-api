open Soup

let parse_result f html =
  try Ok (f (Html.parse html)) with
  | exn -> Error (Error.Parse_error (Printexc.to_string exn))

let require_meta soup prop =
  match Html.meta soup prop with
  | None -> failwith ("missing meta " ^ prop)
  | Some v -> v

let titles_of soup ~default =
  let english = Html.labeled soup "English:" in
  let japanese = Html.labeled soup "Japanese:" in
  let synonyms =
    match Html.labeled soup "Synonyms:" with
    | None -> []
    | Some s -> String.split_on_char ',' s |> List.map Html.trim |> List.filter (fun x -> x <> "")
  in
  let titles =
    ({ Types.type_ = "Default"; title = default } : Types.title)
    :: (match english with
       | None -> []
       | Some t -> [ { Types.type_ = "English"; title = t } ])
    @ (match japanese with
      | None -> []
      | Some t -> [ { Types.type_ = "Japanese"; title = t } ])
    @ List.map (fun t -> ({ Types.type_ = "Synonym"; title = t } : Types.title)) synonyms
  in
  (titles, english, japanese, synonyms)

let synopsis soup =
  match soup $? "p[itemprop=\"description\"], span[itemprop=\"description\"]" with
  | None -> Html.labeled soup "Synopsis:"
  | Some n ->
      let t = Html.texts_of n in
      if String.length t >= 20
         && String.sub t 0 20 = "No synopsis informat"
      then None
      else Some t

let score soup =
  match soup $? "span[itemprop=\"ratingValue\"]" with
  | Some n -> float_of_string_opt (Html.texts_of n)
  | None -> None

let scored_by soup =
  match soup $? "span[itemprop=\"ratingCount\"]" with
  | Some n ->
      let s = Str.global_replace (Str.regexp "[^0-9]") "" (Html.texts_of n) in
      int_of_string_opt s
  | None -> Html.labeled_int soup "Scored by:"

let approved soup =
  match soup $? "#addtolist" with
  | None -> true
  | Some n ->
      not
        (Re.execp
           (Re.compile (Re.str "pending approval"))
           (Html.texts_of n))

let background soup =
  match Html.labeled soup "Background:" with
  | Some t
    when not
           (Re.execp
              (Re.compile (Re.str "No background information"))
              t) ->
      Some t
  | _ -> None

let relations soup =
  let from_table =
    soup $$ "table.entries-table tr"
    |> to_list
    |> List.filter_map (fun tr ->
           match tr $? "td" with
           | None -> None
           | Some td ->
               let rel =
                 Html.texts_of td |> Str.global_replace (Str.regexp ":") "" |> Html.trim
               in
               let entry =
                 tr $$ "td a" |> to_list |> List.filter_map Html.mal_url_of_anchor
               in
               if rel = "" then None
               else Some ({ Types.relation = rel; entry } : Types.relation))
  in
  from_table

let external_links soup =
  soup $$ "div.external_links a.link"
  |> to_list
  |> List.filter_map (fun a ->
         match Html.attr_opt a "href" with
         | None -> None
         | Some href ->
             Some
               ({ Types.name = Html.texts_of a; url = Html.abs_url href }
                 : Types.external_link))

let streaming_links soup =
  soup $$ "div.broadcast a"
  |> to_list
  |> List.filter_map (fun a ->
         match Html.attr_opt a "href" with
         | None -> None
         | Some href ->
             Some
               ({ Types.name = Html.texts_of a; url = Html.abs_url href }
                 : Types.external_link))

let theme_rows soup klass =
  soup $$ Printf.sprintf "div.theme-songs.%s tr, div.js-theme-songs.%s tr" klass klass
  |> to_list
  |> List.map Html.texts_of
  |> List.filter (fun t ->
         t <> ""
         && not
              (Re.execp
                 (Re.compile (Re.str "No opening themes"))
                 t
              || Re.execp (Re.compile (Re.str "No ending themes")) t))

let trailer soup =
  match soup $? "div.video-promotion a, a.iframe" with
  | None -> Types.empty_trailer
  | Some a -> (
      match Html.attr_opt a "href" with
      | None -> Types.empty_trailer
      | Some href -> Html.youtube_from_href href)

let anime soup =
  let url = require_meta soup "og:url" in
  let title = require_meta soup "og:title" in
  let mal_id =
    match Html.id_from_url url with
    | Some id -> id
    | None -> failwith "anime id"
  in
  let titles, title_english, title_japanese, title_synonyms = titles_of soup ~default:title in
  let status = Html.labeled soup "Status:" in
  let premiered = Html.labeled soup "Premiered:" in
  let season, year = Html.parse_season_year premiered in
  ({
     mal_id;
     url;
     images = Html.og_images soup;
     trailer = trailer soup;
     approved = approved soup;
     titles;
     title;
     title_english;
     title_japanese;
     title_synonyms;
     type_ = Html.labeled soup "Type:";
     source = Html.labeled soup "Source:";
     episodes = Html.labeled_int soup "Episodes:";
     status;
     airing =
       (match status with Some "Currently Airing" -> true | _ -> false);
     aired = Html.parse_date_range (Html.labeled soup "Aired:");
     duration = Html.labeled soup "Duration:";
     rating = Html.labeled soup "Rating:";
     score = score soup;
     scored_by = scored_by soup;
     rank = Html.labeled_int soup "Ranked:";
     popularity = Html.labeled_int soup "Popularity:";
     members = Html.labeled_int soup "Members:";
     favorites = Html.labeled_int soup "Favorites:";
     synopsis = synopsis soup;
     background = background soup;
     season;
     year;
     broadcast = Html.parse_broadcast (Html.labeled soup "Broadcast:");
     producers = Html.labeled_urls soup "Producers:";
     licensors = Html.labeled_urls soup "Licensors:";
     studios = Html.labeled_urls soup "Studios:";
     genres = Html.labeled_urls_any soup [ "Genres:"; "Genre:" ];
     explicit_genres =
       Html.labeled_urls_any soup [ "Explicit Genres:"; "Explicit Genre:" ];
     themes = Html.labeled_urls_any soup [ "Themes:"; "Theme:" ];
     demographics = Html.labeled_urls_any soup [ "Demographics:"; "Demographic:" ];
   }
    : Types.anime)

let anime_full soup =
  ({
     anime = anime soup;
     relations = relations soup;
     theme = (theme_rows soup "opnening" @ theme_rows soup "opening", theme_rows soup "ending");
     external_links = external_links soup;
     streaming = streaming_links soup;
   }
    : Types.anime_full)

let manga soup =
  let url = require_meta soup "og:url" in
  let title = require_meta soup "og:title" in
  let mal_id =
    match Html.id_from_url url with Some id -> id | None -> failwith "manga id"
  in
  let titles, title_english, title_japanese, title_synonyms = titles_of soup ~default:title in
  let status = Html.labeled soup "Status:" in
  ({
     mal_id;
     url;
     images = Html.og_images soup;
     approved = approved soup;
     titles;
     title;
     title_english;
     title_japanese;
     title_synonyms;
     type_ = Html.labeled soup "Type:";
     chapters = Html.labeled_int soup "Chapters:";
     volumes = Html.labeled_int soup "Volumes:";
     status;
     publishing =
       (match status with Some "Publishing" -> true | _ -> false);
     published = Html.parse_date_range (Html.labeled soup "Published:");
     score = score soup;
     scored_by = scored_by soup;
     rank = Html.labeled_int soup "Ranked:";
     popularity = Html.labeled_int soup "Popularity:";
     members = Html.labeled_int soup "Members:";
     favorites = Html.labeled_int soup "Favorites:";
     synopsis = synopsis soup;
     background = background soup;
     authors = Html.labeled_urls soup "Authors:";
     serializations = Html.labeled_urls soup "Serialization:";
     genres = Html.labeled_urls_any soup [ "Genres:"; "Genre:" ];
     explicit_genres =
       Html.labeled_urls_any soup [ "Explicit Genres:"; "Explicit Genre:" ];
     themes = Html.labeled_urls_any soup [ "Themes:"; "Theme:" ];
     demographics = Html.labeled_urls_any soup [ "Demographics:"; "Demographic:" ];
   }
    : Types.manga)

let manga_full soup =
  ({
     manga = manga soup;
     relations = relations soup;
     external_links = external_links soup;
   }
    : Types.manga_full)

let character soup =
  let url = require_meta soup "og:url" in
  let name = require_meta soup "og:title" in
  let mal_id =
    match Html.id_from_url url with Some id -> id | None -> failwith "character id"
  in
  let nicknames =
    match Html.labeled soup "Nicknames:" with
    | None -> []
    | Some s ->
        String.split_on_char ',' s |> List.map Html.trim |> List.filter (fun x -> x <> "")
  in
  ({
     mal_id;
     url;
     images = Html.og_images soup;
     name;
     name_kanji = Html.labeled soup "Name:";
     nicknames;
     favorites = Html.labeled_int soup "Member Favorites:";
     about =
       (match soup $? "div.people-informantion-more, td[valign=top] > div" with
       | Some n -> Some (Html.texts_of n)
       | None -> None);
   }
    : Types.character)

let person soup =
  let url = require_meta soup "og:url" in
  let name = require_meta soup "og:title" in
  let mal_id =
    match Html.id_from_url url with Some id -> id | None -> failwith "person id"
  in
  ({
     mal_id;
     url;
     website_url = Html.labeled soup "Website:";
     images = Html.og_images soup;
     name;
     given_name = Html.labeled soup "Given name:";
     family_name = Html.labeled soup "Family name:";
     alternate_names =
       (match Html.labeled soup "Alternate names:" with
       | None -> []
       | Some s -> String.split_on_char ',' s |> List.map Html.trim);
     birthday = Html.parse_date_range (Html.labeled soup "Birthday:");
     favorites = Html.labeled_int soup "Member Favorites:";
     about =
       (match soup $? "div.people-informantion-more" with
       | Some n -> Some (Html.texts_of n)
       | None -> None);
   }
    : Types.person)

let person_from_anchor a =
  match Html.mal_url_of_anchor a with
  | None -> None
  | Some u ->
      let img =
        match a $? "img" with
        | Some i -> (
            match Html.attr_opt i "data-src" |> fun x -> Option.fold ~none:(Html.attr_opt i "src") ~some:Option.some x with
            | Some src -> Html.images_from_url src
            | None -> Types.empty_image_pair)
        | None -> Types.empty_image_pair
      in
      Some
        ({
           Types.mal_id = (u : Types.mal_url).mal_id;
           url = u.url;
           website_url = None;
           images = img;
           name = u.name;
           given_name = None;
           family_name = None;
           alternate_names = [];
           birthday = Types.empty_date_range;
           favorites = None;
           about = None;
         }
          : Types.person)

let character_from_anchor a =
  match Html.mal_url_of_anchor a with
  | None -> None
  | Some u ->
      let img =
        match a $? "img" with
        | Some i -> (
            match
              Option.fold ~none:(Html.attr_opt i "src") ~some:Option.some
                (Html.attr_opt i "data-src")
            with
            | Some src -> Html.images_from_url src
            | None -> Types.empty_image_pair)
        | None -> Types.empty_image_pair
      in
      Some
        ({
           Types.mal_id = (u : Types.mal_url).mal_id;
           url = u.url;
           images = img;
           name = u.name;
           name_kanji = None;
           nicknames = [];
           favorites = None;
           about = None;
         }
          : Types.character)

let anime_characters soup =
  soup $$ "table.js-anime-character-table, table.js-manga-character-table, table[width=\"100%\"]"
  |> to_list
  |> List.concat_map (fun table ->
         table $$ "tr" |> to_list
         |> List.filter_map (fun tr ->
                match tr $? "a[href*=\"/character/\"]" with
                | None -> None
                | Some a ->
                    Option.map
                      (fun character ->
                        let role =
                          match tr $? "small, div.spaceit_pad small" with
                          | Some n -> Html.texts_of n
                          | None -> "Main"
                        in
                        let voice_actors =
                          tr $$ "a[href*=\"/people/\"]"
                          |> to_list
                          |> List.filter_map (fun pa ->
                                 Option.map
                                   (fun p ->
                                     let lang =
                                       match parent pa with
                                       | Some par -> (
                                           match par $? "small" with
                                           | Some s -> Html.texts_of s
                                           | None -> "Japanese")
                                       | None -> "Japanese"
                                     in
                                     (p, lang))
                                   (person_from_anchor pa))
                        in
                        ({
                           Types.character;
                           role;
                           favorites = None;
                           voice_actors;
                         }
                          : Types.character_role))
                      (character_from_anchor a)))

let anime_staff soup =
  soup $$ "a[href*=\"/people/\"]"
  |> to_list
  |> List.filter_map (fun a ->
         match parent a with
         | None -> None
         | Some p ->
             if
               Re.execp
                 (Re.compile (Re.str "character"))
                 (Html.texts_of p |> String.lowercase_ascii)
             then None
             else
               Option.map
                 (fun person ->
                   let positions =
                     match p $? "small" with
                     | Some s ->
                         String.split_on_char ',' (Html.texts_of s)
                         |> List.map Html.trim
                     | None -> []
                   in
                   ({ Types.person; positions } : Types.staff_entry))
                 (person_from_anchor a))

let episodes soup =
  soup $$ "table.episode_list tr.episode-list-data, table.episode_list tr"
  |> to_list
  |> List.filter_map (fun tr ->
         let cells = tr $$ "td" |> to_list in
         match cells with
         | num :: title_td :: _ ->
             let mal_id = int_of_string_opt (Html.texts_of num) in
             let title_a = title_td $? "a" in
             let title =
               match title_a with Some a -> Html.texts_of a | None -> Html.texts_of title_td
             in
             if title = "" || title = "Episode Title" then None
             else
               let url =
                 Option.bind title_a (fun a -> Html.attr_opt a "href")
                 |> Option.map Html.abs_url
               in
               Some
                 ({
                    Types.mal_id = Option.value mal_id ~default:0;
                    url;
                    title;
                    title_japanese = None;
                    title_romanji = None;
                    aired = None;
                    score = None;
                    filler =
                      Re.execp (Re.compile (Re.str "Filler")) (Html.texts_of tr);
                    recap =
                      Re.execp (Re.compile (Re.str "Recap")) (Html.texts_of tr);
                    forum_url = None;
                  }
                   : Types.episode)
         | _ -> None)

let episode_page soup =
  match episodes soup with
  | e :: _ -> e
  | [] ->
      let title = Option.value (Html.meta soup "og:title") ~default:"" in
      ({
         mal_id = 0;
         url = Html.meta soup "og:url";
         title;
         title_japanese = None;
         title_romanji = None;
         aired = None;
         score = None;
         filler = false;
         recap = false;
         forum_url = None;
       }
        : Types.episode)

let news soup =
  soup $$ "div.news-list .news-unit, div.news-unit"
  |> to_list
  |> List.filter_map (fun n ->
         match n $? "p.title a, a.title, a[href*=\"/news/\"]" with
         | None -> None
         | Some a ->
             let url = Option.fold ~none:"" ~some:Html.abs_url (Html.attr_opt a "href") in
             let mal_id =
               match Html.id_from_url url with Some id -> Mal_id.to_int id | None -> 0
             in
             let img =
               match n $? "img" with
               | Some i ->
                   Option.fold ~none:Types.empty_image_pair ~some:Html.images_from_url
                     (Option.fold ~none:(Html.attr_opt i "src") ~some:Option.some
                        (Html.attr_opt i "data-src"))
               | None -> Types.empty_image_pair
             in
             Some
               ({
                  Types.mal_id;
                  url;
                  title = Html.texts_of a;
                  date =
                    Option.map Html.texts_of (n $? "p.information, span.date");
                  author_username = None;
                  author_url = None;
                  forum_url = None;
                  images = img;
                  comments = None;
                  excerpt = Option.map Html.texts_of (n $? "div.text, p.text");
                }
                 : Types.news_item))

let forum soup =
  soup $$ "table.forum_topic tr, table tr"
  |> to_list
  |> List.filter_map (fun tr ->
         match tr $? "a[href*=\"/forum/\"]" with
         | None -> None
         | Some a ->
             let url = Option.fold ~none:"" ~some:Html.abs_url (Html.attr_opt a "href") in
             let mal_id =
               match Html.id_from_url url with Some id -> Mal_id.to_int id | None -> 0
             in
             Some
               ({
                  Types.mal_id;
                  url;
                  title = Html.texts_of a;
                  date = None;
                  author_username = None;
                  author_url = None;
                  comments = None;
                }
                 : Types.forum_topic))

let videos soup =
  let promo =
    soup $$ "div.video-list-outer, div.video-block a.video-list, a.iframe"
    |> to_list
    |> List.filter_map (fun a ->
           match Html.attr_opt a "href" with
           | None -> None
           | Some href ->
               if Re.execp (Re.compile (Re.str "youtu")) href then
                 Some
                   ({
                      Types.title = Html.texts_of a;
                      trailer = Html.youtube_from_href href;
                    }
                     : Types.promo_video)
               else None)
  in
  ({ Types.promo; episodes = []; music_videos = [] } : Types.videos)

let pictures soup =
  soup $$ "div.picSurround img, table img"
  |> to_list
  |> List.filter_map (fun i ->
         match
           Option.fold ~none:(Html.attr_opt i "src") ~some:Option.some
             (Html.attr_opt i "data-src")
         with
         | None -> None
         | Some src ->
             let pair = Html.images_from_url src in
             Some ({ Types.jpg = pair.jpg; webp = pair.webp } : Types.picture))

let int_cell soup labels =
  let rec go = function
    | [] -> None
    | label :: rest -> (
        match Html.labeled_int soup label with Some n -> Some n | None -> go rest)
  in
  go labels

let anime_stats soup =
  ({
     watching = int_cell soup [ "Watching:" ];
     completed = int_cell soup [ "Completed:" ];
     on_hold = int_cell soup [ "On-Hold:" ];
     dropped = int_cell soup [ "Dropped:" ];
     plan_to_watch = int_cell soup [ "Plan to Watch:" ];
     total = int_cell soup [ "Total:" ];
     scores = [];
   }
    : Types.anime_statistics)

let manga_stats soup =
  ({
     reading = int_cell soup [ "Reading:" ];
     completed = int_cell soup [ "Completed:" ];
     on_hold = int_cell soup [ "On-Hold:" ];
     dropped = int_cell soup [ "Dropped:" ];
     plan_to_read = int_cell soup [ "Plan to Read:" ];
     total = int_cell soup [ "Total:" ];
     scores = [];
   }
    : Types.manga_statistics)

let moreinfo soup =
  match soup $? "h2" with
  | _ ->
      soup $$ "h2"
      |> to_list
      |> List.find_opt (fun n -> Html.texts_of n = "More Info")
      |> fun h ->
      match h with
      | None -> None
      | Some n -> (
          match next_sibling n with
          | Some s -> Some (Html.texts_of s)
          | None -> None)

let recommendations soup =
  soup $$ "div.borderClass, div#content div[class*=\"spaceit\"]"
  |> to_list
  |> List.filter_map (fun n ->
         match n $? "a[href*=\"/anime/\"], a[href*=\"/manga/\"]" with
         | None -> None
         | Some a ->
             Option.map
               (fun entry ->
                 let votes =
                   match n $? "strong, a.button_add, span.users" with
                   | Some s -> int_of_string_opt (Str.global_replace (Str.regexp "[^0-9]") "" (Html.texts_of s))
                   | None -> None
                 in
                 let images =
                   match n $? "img" with
                   | Some i ->
                       Option.fold ~none:Types.empty_image_pair ~some:Html.images_from_url
                         (Option.fold ~none:(Html.attr_opt i "src") ~some:Option.some
                            (Html.attr_opt i "data-src"))
                   | None -> Types.empty_image_pair
                 in
                 ({
                    Types.mal_id =
                      string_of_int (Mal_id.to_int (entry : Types.mal_url).mal_id);
                    entry = (entry, images);
                    url = Html.attr_opt a "href" |> Option.map Html.abs_url;
                    votes;
                  }
                   : Types.recommendation))
               (Html.mal_url_of_anchor a))

let reviews soup =
  soup $$ "div.review-element, div.borderDark"
  |> to_list
  |> List.filter_map (fun n ->
         match n $? "a[href*=\"/reviews.php\"], a[href*=\"/profile/\"]" with
         | None -> None
         | Some _ ->
             let user_a = n $? "a[href*=\"/profile/\"]" in
             let username =
               match user_a with Some a -> Html.texts_of a | None -> ""
             in
             let user_url =
               match user_a with
               | Some a ->
                   Option.fold ~none:"" ~some:Html.abs_url (Html.attr_opt a "href")
               | None -> ""
             in
             let text = Option.map Html.texts_of (n $? "div.text, div.body, div.review-body") in
             Some
               ({
                  Types.mal_id = 0;
                  url = None;
                  type_ = None;
                  reactions = [];
                  date = Option.map Html.texts_of (n $? "div.update_at, span.review-date");
                  review = text;
                  score =
                    Option.bind (n $? "span.score, div.rating") (fun s ->
                        int_of_string_opt (Html.texts_of s));
                  tags = [];
                  is_spoiler =
                    Re.execp (Re.compile (Re.str "spoiler")) (Html.texts_of n |> String.lowercase_ascii);
                  is_preliminary = false;
                  episodes_watched = None;
                  chapters_read = None;
                  user = (username, user_url, Types.empty_image_pair);
                }
                 : Types.review))

let search_rows soup kind =
  soup $$ "div.js-categories-seasonal .seasonal-anime, table a.hoverinfo_trigger, div.list table tr"
  |> to_list
  |> List.filter_map (fun n ->
         let a =
           n $? Printf.sprintf "a[href*=\"/%s/\"]" kind
         in
         match a with
         | None -> None
         | Some a -> Html.mal_url_of_anchor a)

let seasonal soup = search_rows soup "anime"

let top_list soup kind = search_rows soup kind

let genres soup =
  soup $$ "a[href*=\"/genre/\"]"
  |> to_list
  |> List.filter_map Html.mal_url_of_anchor

let producers soup =
  soup $$ "a[href*=\"/producer/\"]"
  |> to_list
  |> List.filter_map (fun a ->
         Option.map
           (fun u ->
             ({
                Types.mal_id = (u : Types.mal_url).mal_id;
                url = u.url;
                titles = [ { Types.type_ = "Default"; title = u.name } ];
                images = Types.empty_image_pair;
                established = None;
                about = None;
                count = None;
              }
               : Types.producer))
           (Html.mal_url_of_anchor a))

let producer soup =
  let url = Option.value (Html.meta soup "og:url") ~default:"" in
  let title = Option.value (Html.meta soup "og:title") ~default:"" in
  let mal_id =
    match Html.id_from_url url with Some id -> id | None -> Mal_id.of_string "1" |> Option.get
  in
  ({
     mal_id;
     url;
     titles = [ { Types.type_ = "Default"; title } ];
     images = Html.og_images soup;
     established = Html.labeled soup "Established:";
     about = synopsis soup;
     count = Html.labeled_int soup "Favorites:";
   }
    : Types.producer)

let magazines soup =
  soup $$ "a[href*=\"/magazine/\"]"
  |> to_list
  |> List.filter_map Html.mal_url_of_anchor

let club soup =
  let url = Option.value (Html.meta soup "og:url") ~default:"" in
  let name = Option.value (Html.meta soup "og:title") ~default:"" in
  let mal_id =
    match Html.id_from_url url with
    | Some id -> id
    | None -> (
        match Html.labeled_int soup "Club ID:" with
        | Some n -> Option.value (Mal_id.of_int n) ~default:(Option.get (Mal_id.of_int 1))
        | None -> Option.get (Mal_id.of_int 1))
  in
  ({
     mal_id;
     url;
     images = Html.og_images soup;
     name;
     members = Html.labeled_int soup "Members:";
     category = Html.labeled soup "Category:";
     created = Html.labeled soup "Created:";
     access = Html.labeled soup "Club access:";
   }
    : Types.club)

let user_profile soup username =
  let url = Printf.sprintf "https://myanimelist.net/profile/%s" username in
  ({
     Types.mal_id = Html.labeled_int soup "MAL ID:";
     username;
     url;
     images = Html.og_images soup;
     last_online = Html.labeled soup "Last Online:";
     gender = Html.labeled soup "Gender:";
     birthday = Html.labeled soup "Birthday:";
     location = Html.labeled soup "Location:";
     joined = Html.labeled soup "Joined:";
   }
    : Types.user_profile)

let user_stats soup =
  ({
     Types.days_watched = Html.labeled_float soup "Days:";
     mean_score = Html.labeled_float soup "Mean Score:";
     watching = Html.labeled_int soup "Watching:";
     completed = Html.labeled_int soup "Completed:";
     on_hold = Html.labeled_int soup "On-Hold:";
     dropped = Html.labeled_int soup "Dropped:";
     plan_to_watch = Html.labeled_int soup "Plan to Watch:";
     total_entries = Html.labeled_int soup "Total Entries:";
     rewatched = Html.labeled_int soup "Rewatched:";
     episodes_watched = Html.labeled_int soup "Episodes:";
     days_read = None;
     reading = Html.labeled_int soup "Reading:";
     plan_to_read = Html.labeled_int soup "Plan to Read:";
     reread = Html.labeled_int soup "Reread:";
     chapters_read = Html.labeled_int soup "Chapters:";
     volumes_read = Html.labeled_int soup "Volumes:";
   }
    : Types.user_stats)

let watch_episodes soup =
  soup $$ "div.video-block, div.watch-info"
  |> to_list
  |> List.filter_map (fun n ->
         match n $? "a[href*=\"/anime/\"]" with
         | None -> None
         | Some a ->
             Option.map
               (fun entry ->
                 let images =
                   match n $? "img" with
                   | Some i ->
                       Option.fold ~none:Types.empty_image_pair ~some:Html.images_from_url
                         (Option.fold ~none:(Html.attr_opt i "src") ~some:Option.some
                            (Html.attr_opt i "data-src"))
                   | None -> Types.empty_image_pair
                 in
                 ({
                    Types.entry = (entry, images);
                    episodes = [];
                    region_locked = false;
                  }
                   : Types.watch_episode))
               (Html.mal_url_of_anchor a))

let character_anime soup =
  soup $$ "table a[href*=\"/anime/\"]"
  |> to_list
  |> List.filter_map Html.mal_url_of_anchor

let character_manga soup =
  soup $$ "table a[href*=\"/manga/\"]"
  |> to_list
  |> List.filter_map Html.mal_url_of_anchor

let character_voices soup =
  soup $$ "a[href*=\"/people/\"]"
  |> to_list
  |> List.filter_map person_from_anchor

let parse_anime html = parse_result anime html
let parse_anime_full html = parse_result anime_full html
let parse_manga html = parse_result manga html
let parse_manga_full html = parse_result manga_full html
let parse_character html = parse_result character html
let parse_person html = parse_result person html
let parse_anime_characters html = parse_result anime_characters html
let parse_anime_staff html = parse_result anime_staff html
let parse_episodes html = parse_result episodes html
let parse_episode html = parse_result episode_page html
let parse_news html = parse_result news html
let parse_forum html = parse_result forum html
let parse_videos html = parse_result videos html
let parse_pictures html = parse_result pictures html
let parse_anime_stats html = parse_result anime_stats html
let parse_manga_stats html = parse_result manga_stats html
let parse_moreinfo html = parse_result moreinfo html
let parse_recommendations html = parse_result recommendations html
let parse_reviews html = parse_result reviews html
let parse_seasonal html = parse_result seasonal html
let parse_top kind html = parse_result (fun s -> top_list s kind) html
let parse_genres html = parse_result genres html
let parse_producers html = parse_result producers html
let parse_producer html = parse_result producer html
let parse_magazines html = parse_result magazines html
let parse_club html = parse_result club html
let parse_user html username = parse_result (fun s -> user_profile s username) html
let parse_user_stats html = parse_result user_stats html
let parse_watch html = parse_result watch_episodes html
let parse_character_anime html = parse_result character_anime html
let parse_character_manga html = parse_result character_manga html
let parse_character_voices html = parse_result character_voices html
let parse_relations html = parse_result relations html
let parse_external html = parse_result external_links html
let parse_streaming html = parse_result streaming_links html
let parse_themes html =
  parse_result (fun s -> (theme_rows s "opnening" @ theme_rows s "opening", theme_rows s "ending")) html
let parse_search kind html = parse_result (fun s -> search_rows s kind) html
