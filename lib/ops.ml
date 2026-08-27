let wrap_list page per_page xs encode =
  let count = List.length xs in
  Json.page
    ~pagination:
      {
        last_visible_page = (if count >= per_page then page + 1 else max 1 page);
        has_next_page = count >= per_page;
        current_page = page;
        items =
          Some
            {
              count;
              total = ((page - 1) * per_page) + count;
              per_page;
            };
      }
    ~data:(`List (List.map encode xs))

let mal_url_entry (u : Types.mal_url) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson u.mal_id);
      ("url", `String u.url);
      ("images", Types.image_pair_to_yojson Types.empty_image_pair);
      ("title", `String u.name);
      ("name", `String u.name);
    ]

let fetch_data app ~url ~ttl ~parse ~encode =
  App.fetch app ~url ~ttl ~parse ~encode:(fun x -> Json.data (encode x))

let search app ~kind ~url ~ttl ~q ~page =
  if String.trim q = "" then
    Lwt.return (Error (Error.Bad_request "The q query parameter is required"))
  else
    App.fetch app ~url:(url q page) ~ttl ~parse:(Parse.parse_search kind)
      ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let root app =
  let hb = app.App.heartbeat in
  `Assoc
    [
      ("version", `String app.cfg.app_version);
      ("parser_version", `String app.cfg.app_version);
      ("github_url", `String app.cfg.github_url);
      ( "myanimelist_heartbeat",
        `Assoc
          [
            ("status", `String (Heartbeat.status hb));
            ("score", `Float (Heartbeat.score_value hb));
            ("down", `Bool (Heartbeat.is_down hb));
            ("last_downtime", Json.opt_string (Heartbeat.last_downtime hb));
          ] );
    ]

let search_anime app ~q ~page =
  search app ~kind:"anime" ~url:Mal_url.search_anime ~ttl:app.cfg.cache_search_expire ~q ~page

let get_anime app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_anime
    ~encode:Types.anime_to_yojson

let get_anime_full app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_anime_full ~encode:Types.anime_full_to_yojson

let get_anime_characters app id =
  fetch_data app ~url:(Mal_url.anime_characters id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_anime_characters ~encode:(fun xs -> Json.list Types.character_role_to_yojson xs)

let get_anime_staff app id =
  fetch_data app ~url:(Mal_url.anime_characters id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_anime_staff ~encode:(fun xs -> Json.list Types.staff_entry_to_yojson xs)

let get_anime_episodes app id ~page =
  App.fetch app ~url:(Mal_url.anime_episodes id page) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_episodes ~encode:(fun xs ->
        wrap_list page app.cfg.max_results_per_page xs Types.episode_to_yojson)

let get_anime_episode app id episode_id =
  fetch_data app ~url:(Mal_url.anime_episode id episode_id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_episode ~encode:Types.episode_to_yojson

let get_anime_news app id ~page =
  App.fetch app ~url:(Mal_url.anime_news id page) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_news
    ~encode:(fun xs -> wrap_list page app.cfg.max_results_per_page xs Types.news_item_to_yojson)

let get_anime_forum app id =
  fetch_data app ~url:(Mal_url.anime_forum id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_forum
    ~encode:(fun xs -> Json.list Types.forum_topic_to_yojson xs)

let get_anime_videos app id =
  fetch_data app ~url:(Mal_url.anime_videos id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_videos
    ~encode:Types.videos_to_yojson

let get_anime_video_episodes app id =
  fetch_data app ~url:(Mal_url.anime_videos id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_videos
    ~encode:(fun v -> Json.list Types.video_episode_to_yojson v.Types.episodes)

let get_anime_pictures app id =
  fetch_data app ~url:(Mal_url.anime_pictures id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_pictures ~encode:(fun xs -> Json.list Types.picture_to_yojson xs)

let get_anime_statistics app id =
  fetch_data app ~url:(Mal_url.anime_stats id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_anime_stats ~encode:Types.anime_statistics_to_yojson

let get_anime_moreinfo app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_moreinfo
    ~encode:(fun s -> `Assoc [ ("moreinfo", Json.opt_string s) ])

let get_anime_recommendations app id =
  fetch_data app ~url:(Mal_url.anime_recs id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_recommendations ~encode:(fun xs -> Json.list Types.recommendation_to_yojson xs)

let get_anime_userupdates app id =
  fetch_data app ~url:(Mal_url.anime_stats id) ~ttl:app.cfg.cache_user_expire
    ~parse:(fun _ -> Ok [])
    ~encode:(fun xs -> wrap_list 1 25 xs Types.user_update_to_yojson)

let get_anime_reviews app id ~page =
  App.fetch app ~url:(Mal_url.anime_reviews id page) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_reviews ~encode:(fun xs ->
        wrap_list page app.cfg.max_results_per_page xs Types.review_to_yojson)

let get_anime_relations app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_relations
    ~encode:(fun xs -> Json.list Types.relation_to_yojson xs)

let get_anime_themes app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_themes
    ~encode:(fun (op, ed) ->
        `Assoc [ ("openings", Json.string_list op); ("endings", Json.string_list ed) ])

let get_anime_external app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_external
    ~encode:(fun xs -> Json.list Types.external_link_to_yojson xs)

let get_anime_streaming app id =
  fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_streaming
    ~encode:(fun xs -> Json.list Types.external_link_to_yojson xs)

let search_manga app ~q ~page =
  search app ~kind:"manga" ~url:Mal_url.search_manga ~ttl:app.cfg.cache_search_expire ~q ~page

let get_manga app id =
  fetch_data app ~url:(Mal_url.manga id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_manga
    ~encode:Types.manga_to_yojson

let get_manga_full app id =
  fetch_data app ~url:(Mal_url.manga id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_manga_full
    ~encode:Types.manga_full_to_yojson

let get_manga_characters app id =
  fetch_data app ~url:(Mal_url.manga_characters id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_anime_characters ~encode:(fun xs -> Json.list Types.character_role_to_yojson xs)

let get_manga_news app id ~page =
  App.fetch app ~url:(Mal_url.manga_news id page) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_news
    ~encode:(fun xs -> wrap_list page app.cfg.max_results_per_page xs Types.news_item_to_yojson)

let get_manga_forum app id =
  fetch_data app ~url:(Mal_url.manga_forum id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_forum
    ~encode:(fun xs -> Json.list Types.forum_topic_to_yojson xs)

let get_manga_pictures app id =
  fetch_data app ~url:(Mal_url.manga_pictures id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_pictures ~encode:(fun xs -> Json.list Types.picture_to_yojson xs)

let get_manga_statistics app id =
  fetch_data app ~url:(Mal_url.manga_stats id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_manga_stats ~encode:Types.manga_statistics_to_yojson

let get_manga_moreinfo app id =
  fetch_data app ~url:(Mal_url.manga id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_moreinfo
    ~encode:(fun s -> `Assoc [ ("moreinfo", Json.opt_string s) ])

let get_manga_recommendations app id =
  fetch_data app ~url:(Mal_url.manga_recs id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_recommendations ~encode:(fun xs -> Json.list Types.recommendation_to_yojson xs)

let get_manga_userupdates app id =
  fetch_data app ~url:(Mal_url.manga_stats id) ~ttl:app.cfg.cache_user_expire
    ~parse:(fun _ -> Ok [])
    ~encode:(fun xs -> wrap_list 1 25 xs Types.user_update_to_yojson)

let get_manga_reviews app id ~page =
  App.fetch app ~url:(Mal_url.manga_reviews id page) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_reviews ~encode:(fun xs ->
        wrap_list page app.cfg.max_results_per_page xs Types.review_to_yojson)

let get_manga_relations app id =
  fetch_data app ~url:(Mal_url.manga id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_relations
    ~encode:(fun xs -> Json.list Types.relation_to_yojson xs)

let get_manga_external app id =
  fetch_data app ~url:(Mal_url.manga id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_external
    ~encode:(fun xs -> Json.list Types.external_link_to_yojson xs)

let search_characters app ~q ~page =
  search app ~kind:"character" ~url:Mal_url.search_character ~ttl:app.cfg.cache_search_expire ~q ~page

let get_character app id =
  fetch_data app ~url:(Mal_url.character id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character ~encode:Types.character_to_yojson

let get_character_full app id =
  App.fetch app ~url:(Mal_url.character id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character ~encode:(fun c ->
        Json.data
          (match Types.character_to_yojson c with
          | `Assoc fields ->
              `Assoc (fields @ [ ("anime", `List []); ("manga", `List []); ("voices", `List []) ])
          | json -> json))

let get_character_anime app id =
  fetch_data app ~url:(Mal_url.character id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character_anime ~encode:(fun xs ->
        Json.list
          (fun u -> `Assoc [ ("role", `String "Main"); ("anime", mal_url_entry u) ])
          xs)

let get_character_manga app id =
  fetch_data app ~url:(Mal_url.character id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character_manga ~encode:(fun xs ->
        Json.list
          (fun u -> `Assoc [ ("role", `String "Main"); ("manga", mal_url_entry u) ])
          xs)

let get_character_voices app id =
  fetch_data app ~url:(Mal_url.character id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character_voices ~encode:(fun xs -> Json.list Types.person_mini_to_yojson xs)

let get_character_pictures app id =
  fetch_data app ~url:(Mal_url.character_pictures id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_pictures ~encode:(fun xs -> Json.list Types.picture_to_yojson xs)

let search_people app ~q ~page =
  search app ~kind:"people" ~url:Mal_url.search_people ~ttl:app.cfg.cache_search_expire ~q ~page

let get_person app id =
  fetch_data app ~url:(Mal_url.people id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_person
    ~encode:Types.person_to_yojson

let get_person_full app id =
  fetch_data app ~url:(Mal_url.people id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_person
    ~encode:Types.person_to_yojson

let get_person_anime app id =
  fetch_data app ~url:(Mal_url.people id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character_anime ~encode:(fun xs -> Json.list mal_url_entry xs)

let get_person_manga app id =
  fetch_data app ~url:(Mal_url.people id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character_manga ~encode:(fun xs -> Json.list mal_url_entry xs)

let get_person_voices app id =
  fetch_data app ~url:(Mal_url.people id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_character_voices ~encode:(fun xs -> Json.list Types.person_to_yojson xs)

let get_person_pictures app id =
  fetch_data app ~url:(Mal_url.people_pictures id) ~ttl:app.cfg.cache_default_expire
    ~parse:Parse.parse_pictures ~encode:(fun xs -> Json.list Types.picture_to_yojson xs)

let get_seasons app =
  fetch_data app ~url:Mal_url.season_archive ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal
    ~encode:(fun xs -> Json.list mal_url_entry xs)

let get_seasons_now app ~page =
  App.fetch app ~url:Mal_url.season_now ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_seasons_upcoming app ~page =
  App.fetch app ~url:Mal_url.season_upcoming ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_season app ~year ~season ~page =
  App.fetch app ~url:(Mal_url.season year season) ~ttl:app.cfg.cache_meta_expire
    ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_schedules app ~page =
  App.fetch app ~url:Mal_url.schedule ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_schedules_filter app ~filter:_ ~page = get_schedules app ~page

let get_producers app ~page =
  App.fetch app ~url:Mal_url.producers ~ttl:app.cfg.cache_producers_expire ~parse:Parse.parse_producers
    ~encode:(fun xs -> wrap_list page 25 xs Types.producer_to_yojson)

let get_producer app id =
  fetch_data app ~url:(Mal_url.producer id) ~ttl:app.cfg.cache_producers_expire
    ~parse:Parse.parse_producer ~encode:Types.producer_to_yojson

let get_producer_full app id = get_producer app id

let get_producer_external app id =
  fetch_data app ~url:(Mal_url.producer id) ~ttl:app.cfg.cache_producers_expire
    ~parse:Parse.parse_external ~encode:(fun xs -> Json.list Types.external_link_to_yojson xs)

let get_magazines app ~page =
  App.fetch app ~url:Mal_url.magazines ~ttl:app.cfg.cache_magazines_expire ~parse:Parse.parse_magazines
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let search_users app ~q ~page =
  search app ~kind:"profile"
    ~url:(fun q _page -> Mal_url.search_users q)
    ~ttl:app.cfg.cache_search_expire ~q ~page

let get_users_recentlyonline () = Json.data (`List [])

let get_user_by_id mal_id =
  Json.data (`Assoc [ ("mal_id", `String mal_id); ("username", `Null) ])

let get_user app username =
  App.fetch app ~url:(Mal_url.user username) ~ttl:app.cfg.cache_user_expire
    ~parse:(fun html -> Parse.parse_user html username)
    ~encode:(fun u -> Json.data (Types.user_profile_to_yojson u))

let get_user_full app username = get_user app username

let get_user_statistics app username =
  fetch_data app ~url:(Mal_url.user_stats username) ~ttl:app.cfg.cache_user_expire
    ~parse:Parse.parse_user_stats ~encode:Types.user_stats_to_yojson

let get_user_favorites app username =
  App.fetch app ~url:(Mal_url.user_favorites username) ~ttl:app.cfg.cache_user_expire
    ~parse:Parse.parse_seasonal ~encode:(fun xs ->
        Json.data
          (`Assoc
             [
               ("anime", Json.list mal_url_entry xs);
               ("manga", `List []);
               ("characters", `List []);
               ("people", `List []);
             ]))

let get_user_userupdates () = Json.data (`Assoc [ ("anime", `List []); ("manga", `List []) ])

let get_user_about app username =
  App.fetch app ~url:(Mal_url.user username) ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_moreinfo
    ~encode:(fun s -> Json.data (`Assoc [ ("about", Json.opt_string s) ]))

let get_user_history () = Json.data (`List [])

let get_user_history_type ~history_type:_ = Json.data (`List [])

let get_user_friends app username ~page =
  App.fetch app ~url:(Mal_url.user_friends username) ~ttl:app.cfg.cache_user_expire
    ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let require_user_lists app f =
  if app.App.cfg.disable_user_lists then
    Lwt.return (Error (Error.Bad_request "User lists are disabled"))
  else f ()

let get_user_animelist app username ~page =
  require_user_lists app (fun () ->
      App.fetch app ~url:(Mal_url.user_animelist username) ~ttl:app.cfg.cache_userlist_expire
        ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry))

let get_user_animelist_status app username ~status:_ ~page = get_user_animelist app username ~page

let get_user_mangalist app username ~page =
  require_user_lists app (fun () ->
      App.fetch app ~url:(Mal_url.user_mangalist username) ~ttl:app.cfg.cache_userlist_expire
        ~parse:(Parse.parse_search "manga") ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry))

let get_user_mangalist_status app _username ~status:_ =
  require_user_lists app (fun () -> Lwt.return (Ok (wrap_list 1 25 [] mal_url_entry)))

let get_user_recommendations app username =
  App.fetch app ~url:(Mal_url.user_recs username) ~ttl:app.cfg.cache_user_expire
    ~parse:Parse.parse_recommendations ~encode:(fun xs ->
        Json.data (Json.list Types.recommendation_to_yojson xs))

let get_user_reviews app username ~page =
  App.fetch app ~url:(Mal_url.user_reviews username) ~ttl:app.cfg.cache_user_expire
    ~parse:Parse.parse_reviews ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)

let get_user_clubs app username ~page =
  App.fetch app ~url:(Mal_url.user_clubs username) ~ttl:app.cfg.cache_user_expire
    ~parse:(Parse.parse_search "club") ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_user_external app username =
  fetch_data app ~url:(Mal_url.user username) ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_external
    ~encode:(fun xs -> Json.list Types.external_link_to_yojson xs)

let get_genres_anime app =
  fetch_data app ~url:Mal_url.genres_anime ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_genres
    ~encode:(fun xs -> Json.list mal_url_entry xs)

let get_genres_manga app =
  fetch_data app ~url:Mal_url.genres_manga ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_genres
    ~encode:(fun xs -> Json.list mal_url_entry xs)

let get_top_anime app ~page =
  App.fetch app
    ~url:(Mal_url.top_anime ^ "?limit=25&page=" ^ string_of_int page)
    ~ttl:app.cfg.cache_meta_expire ~parse:(Parse.parse_top "anime")
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_top_manga app ~page =
  App.fetch app ~url:Mal_url.top_manga ~ttl:app.cfg.cache_meta_expire ~parse:(Parse.parse_top "manga")
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_top_characters app ~page =
  App.fetch app ~url:Mal_url.top_characters ~ttl:app.cfg.cache_meta_expire
    ~parse:(Parse.parse_top "character") ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_top_people app ~page =
  App.fetch app ~url:Mal_url.top_people ~ttl:app.cfg.cache_meta_expire ~parse:(Parse.parse_top "people")
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_top_reviews app ~page =
  App.fetch app ~url:Mal_url.top_reviews ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_reviews
    ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)

let search_clubs app ~q ~page =
  search app ~kind:"club"
    ~url:(fun q _page -> Mal_url.search_clubs q)
    ~ttl:app.cfg.cache_search_expire ~q ~page

let get_club app id =
  fetch_data app ~url:(Mal_url.club id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_club
    ~encode:Types.club_to_yojson

let get_club_members app id ~page =
  App.fetch app ~url:(Mal_url.club id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_seasonal
    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)

let get_club_staff app id =
  fetch_data app ~url:(Mal_url.club id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_anime_staff
    ~encode:(fun xs -> Json.list Types.staff_entry_to_yojson xs)

let get_club_relations app id =
  fetch_data app ~url:(Mal_url.club id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_relations
    ~encode:(fun xs -> Json.list Types.relation_to_yojson xs)

let get_reviews_anime app ~page =
  App.fetch app ~url:Mal_url.reviews_anime ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_reviews
    ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)

let get_reviews_manga app ~page =
  App.fetch app ~url:Mal_url.reviews_manga ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_reviews
    ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)

let get_recommendations_anime app ~page =
  App.fetch app ~url:Mal_url.recs_anime ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_recommendations
    ~encode:(fun xs -> wrap_list page 25 xs Types.recommendation_to_yojson)

let get_recommendations_manga app ~page =
  App.fetch app ~url:Mal_url.recs_manga ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_recommendations
    ~encode:(fun xs -> wrap_list page 25 xs Types.recommendation_to_yojson)

let get_watch_episodes app ~page =
  App.fetch app ~url:Mal_url.watch_episodes ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_watch
    ~encode:(fun xs -> wrap_list page 25 xs Types.watch_episode_to_yojson)

let get_watch_episodes_popular app ~page =
  App.fetch app ~url:(Mal_url.watch_episodes ^ "?popular") ~ttl:app.cfg.cache_meta_expire
    ~parse:Parse.parse_watch ~encode:(fun xs -> wrap_list page 25 xs Types.watch_episode_to_yojson)

let get_watch_promos app ~page =
  App.fetch app ~url:Mal_url.watch_promos ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_videos
    ~encode:(fun v -> wrap_list page 25 v.Types.promo Types.promo_video_to_yojson)

let get_watch_promos_popular app ~page =
  App.fetch app ~url:(Mal_url.watch_promos ^ "?popular") ~ttl:app.cfg.cache_meta_expire
    ~parse:Parse.parse_videos ~encode:(fun v -> wrap_list page 25 v.Types.promo Types.promo_video_to_yojson)

let pick_random xs =
  match xs with
  | [] -> Json.data (`Assoc [])
  | _ ->
      let i = Random.int (List.length xs) in
      Json.data (mal_url_entry (List.nth xs i))

let get_random_anime app =
  App.fetch app ~url:Mal_url.season_now ~ttl:60 ~parse:Parse.parse_seasonal ~encode:pick_random

let get_random_manga app =
  App.fetch app ~url:Mal_url.top_manga ~ttl:60 ~parse:(Parse.parse_top "manga") ~encode:pick_random

let get_random_characters () = Json.data (`Assoc [])

let get_random_people () = Json.data (`Assoc [])

let get_random_users () = Json.data (`Assoc [])
