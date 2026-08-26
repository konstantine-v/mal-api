let json_ok body = Dream.json (Yojson.Safe.to_string body)

let respond = function
  | Ok body -> json_ok body
  | Error e -> Error.respond e

let page_of req =
  match Dream.query req "page" with
  | None | Some "" -> 1
  | Some s -> max 1 (Option.value (int_of_string_opt s) ~default:1)

let q_of req = Option.value (Dream.query req "q") ~default:""

let id_of req =
  match Mal_id.of_string (Dream.param req "id") with
  | Some id -> Ok id
  | None -> Error (Error.Bad_request "id must be a positive integer")

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

let root app _req =
  let hb = app.App.heartbeat in
  json_ok
    (`Assoc
       [
         ("author_url", `String "https://github.com/jikan-me/jikan-rest");
         ("discord_url", `String "http://discord.jikan.moe");
         ("version", `String app.cfg.app_version);
         ("parser_version", `String app.cfg.app_version);
         ("website_url", `String "https://jikan.moe");
         ("documentation_url", `String "https://docs.api.jikan.moe/");
         ("github_url", `String "https://github.com/jikan-me/jikan-rest");
         ("parser_github_url", `String "https://github.com/jikan-me/jikan");
         ("production_api_url", `String "https://api.jikan.moe/v4/");
         ("status_url", `String "https://status.jikan.moe");
         ( "myanimelist_heartbeat",
           `Assoc
             [
               ("status", `String (Heartbeat.status hb));
               ("score", `Float (Heartbeat.score_value hb));
               ("down", `Bool (Heartbeat.is_down hb));
               ("last_downtime", Json.opt_string (Heartbeat.last_downtime hb));
             ] );
       ])

let with_id req f =
  match id_of req with Error e -> Error.respond e | Ok id -> f id

let anime_main app req =
  with_id req (fun id ->
      let%lwt r =
        fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_anime ~encode:Types.anime_to_yojson
      in
      respond r)

let anime_full app req =
  with_id req (fun id ->
      let%lwt r =
        fetch_data app ~url:(Mal_url.anime id) ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_anime_full ~encode:Types.anime_full_to_yojson
      in
      respond r)

let sub_data app req url ttl parse encode =
  with_id req (fun id ->
      let%lwt r = fetch_data app ~url:(url id) ~ttl ~parse ~encode in
      respond r)

let sub_list app req url ttl parse encode =
  with_id req (fun id ->
      let page = page_of req in
      let%lwt r =
        App.fetch app ~url:(url id page) ~ttl ~parse
          ~encode:(fun xs -> wrap_list page app.cfg.max_results_per_page xs encode)
      in
      respond r)

let search app ~kind ~url ~ttl req =
  let q = q_of req in
  let page = page_of req in
  if String.trim q = "" then
    Error.respond (Error.Bad_request "The q query parameter is required")
  else
    let%lwt r =
      App.fetch app ~url:(url q page) ~ttl ~parse:(Parse.parse_search kind)
        ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
    in
    respond r

let handler app =
  Dream.router
    [
      Dream.get "/" (root app);
      Dream.scope "/v4" []
        [
          Dream.get "/anime" (search app ~kind:"anime" ~url:Mal_url.search_anime
             ~ttl:app.cfg.cache_search_expire);
          Dream.get "/anime/:id" (anime_main app);
          Dream.get "/anime/:id/full" (anime_full app);
          Dream.get "/anime/:id/characters" (fun req ->
              sub_data app req Mal_url.anime_characters app.cfg.cache_default_expire
                Parse.parse_anime_characters (fun xs ->
                    Json.list Types.character_role_to_yojson xs));
          Dream.get "/anime/:id/staff" (fun req ->
              sub_data app req Mal_url.anime_characters app.cfg.cache_default_expire
                Parse.parse_anime_staff (fun xs -> Json.list Types.staff_entry_to_yojson xs));
          Dream.get "/anime/:id/episodes" (fun req ->
              sub_list app req Mal_url.anime_episodes app.cfg.cache_default_expire
                Parse.parse_episodes Types.episode_to_yojson);
          Dream.get "/anime/:id/episodes/:episodeId" (fun req ->
              with_id req (fun id ->
                  match int_of_string_opt (Dream.param req "episodeId") with
                  | None -> Error.respond (Error.Bad_request "invalid episode id")
                  | Some ep ->
                      let%lwt r =
                        fetch_data app ~url:(Mal_url.anime_episode id ep)
                          ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_episode ~encode:Types.episode_to_yojson
                      in
                      respond r));
          Dream.get "/anime/:id/news" (fun req ->
              sub_list app req Mal_url.anime_news app.cfg.cache_default_expire
                Parse.parse_news Types.news_item_to_yojson);
          Dream.get "/anime/:id/forum" (fun req ->
              sub_data app req Mal_url.anime_forum app.cfg.cache_default_expire
                Parse.parse_forum (fun xs -> Json.list Types.forum_topic_to_yojson xs));
          Dream.get "/anime/:id/videos" (fun req ->
              sub_data app req Mal_url.anime_videos app.cfg.cache_default_expire
                Parse.parse_videos Types.videos_to_yojson);
          Dream.get "/anime/:id/videos/episodes" (fun req ->
              sub_data app req Mal_url.anime_videos app.cfg.cache_default_expire
                Parse.parse_videos (fun v ->
                    Json.list Types.video_episode_to_yojson v.Types.episodes));
          Dream.get "/anime/:id/pictures" (fun req ->
              sub_data app req Mal_url.anime_pictures app.cfg.cache_default_expire
                Parse.parse_pictures (fun xs -> Json.list Types.picture_to_yojson xs));
          Dream.get "/anime/:id/statistics" (fun req ->
              sub_data app req Mal_url.anime_stats app.cfg.cache_default_expire
                Parse.parse_anime_stats Types.anime_statistics_to_yojson);
          Dream.get "/anime/:id/moreinfo" (fun req ->
              sub_data app req Mal_url.anime app.cfg.cache_default_expire
                Parse.parse_moreinfo (fun s ->
                    `Assoc [ ("moreinfo", Json.opt_string s) ]));
          Dream.get "/anime/:id/recommendations" (fun req ->
              sub_data app req Mal_url.anime_recs app.cfg.cache_default_expire
                Parse.parse_recommendations (fun xs ->
                    Json.list Types.recommendation_to_yojson xs));
          Dream.get "/anime/:id/userupdates" (fun req ->
              sub_data app req Mal_url.anime_stats app.cfg.cache_user_expire
                (fun _ -> Ok [])
                (fun xs -> wrap_list 1 25 xs Types.user_update_to_yojson));
          Dream.get "/anime/:id/reviews" (fun req ->
              sub_list app req Mal_url.anime_reviews app.cfg.cache_default_expire
                Parse.parse_reviews Types.review_to_yojson);
          Dream.get "/anime/:id/relations" (fun req ->
              sub_data app req Mal_url.anime app.cfg.cache_default_expire
                Parse.parse_relations (fun xs -> Json.list Types.relation_to_yojson xs));
          Dream.get "/anime/:id/themes" (fun req ->
              sub_data app req Mal_url.anime app.cfg.cache_default_expire
                Parse.parse_themes (fun (op, ed) ->
                    `Assoc
                      [
                        ("openings", Json.string_list op);
                        ("endings", Json.string_list ed);
                      ]));
          Dream.get "/anime/:id/external" (fun req ->
              sub_data app req Mal_url.anime app.cfg.cache_default_expire
                Parse.parse_external (fun xs -> Json.list Types.external_link_to_yojson xs));
          Dream.get "/anime/:id/streaming" (fun req ->
              sub_data app req Mal_url.anime app.cfg.cache_default_expire
                Parse.parse_streaming (fun xs ->
                    Json.list Types.external_link_to_yojson xs));
          Dream.get "/manga" (search app ~kind:"manga" ~url:Mal_url.search_manga
             ~ttl:app.cfg.cache_search_expire);
          Dream.get "/manga/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.manga id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_manga ~encode:Types.manga_to_yojson
                  in
                  respond r));
          Dream.get "/manga/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.manga id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_manga_full ~encode:Types.manga_full_to_yojson
                  in
                  respond r));
          Dream.get "/manga/:id/characters" (fun req ->
              sub_data app req Mal_url.manga_characters app.cfg.cache_default_expire
                Parse.parse_anime_characters (fun xs ->
                    Json.list Types.character_role_to_yojson xs));
          Dream.get "/manga/:id/news" (fun req ->
              sub_list app req Mal_url.manga_news app.cfg.cache_default_expire
                Parse.parse_news Types.news_item_to_yojson);
          Dream.get "/manga/:id/forum" (fun req ->
              sub_data app req Mal_url.manga_forum app.cfg.cache_default_expire
                Parse.parse_forum (fun xs -> Json.list Types.forum_topic_to_yojson xs));
          Dream.get "/manga/:id/pictures" (fun req ->
              sub_data app req Mal_url.manga_pictures app.cfg.cache_default_expire
                Parse.parse_pictures (fun xs -> Json.list Types.picture_to_yojson xs));
          Dream.get "/manga/:id/statistics" (fun req ->
              sub_data app req Mal_url.manga_stats app.cfg.cache_default_expire
                Parse.parse_manga_stats Types.manga_statistics_to_yojson);
          Dream.get "/manga/:id/moreinfo" (fun req ->
              sub_data app req Mal_url.manga app.cfg.cache_default_expire
                Parse.parse_moreinfo (fun s ->
                    `Assoc [ ("moreinfo", Json.opt_string s) ]));
          Dream.get "/manga/:id/recommendations" (fun req ->
              sub_data app req Mal_url.manga_recs app.cfg.cache_default_expire
                Parse.parse_recommendations (fun xs ->
                    Json.list Types.recommendation_to_yojson xs));
          Dream.get "/manga/:id/userupdates" (fun req ->
              sub_data app req Mal_url.manga_stats app.cfg.cache_user_expire
                (fun _ -> Ok [])
                (fun xs -> wrap_list 1 25 xs Types.user_update_to_yojson));
          Dream.get "/manga/:id/reviews" (fun req ->
              sub_list app req Mal_url.manga_reviews app.cfg.cache_default_expire
                Parse.parse_reviews Types.review_to_yojson);
          Dream.get "/manga/:id/relations" (fun req ->
              sub_data app req Mal_url.manga app.cfg.cache_default_expire
                Parse.parse_relations (fun xs -> Json.list Types.relation_to_yojson xs));
          Dream.get "/manga/:id/external" (fun req ->
              sub_data app req Mal_url.manga app.cfg.cache_default_expire
                Parse.parse_external (fun xs ->
                    Json.list Types.external_link_to_yojson xs));
          Dream.get "/characters" (search app ~kind:"character"
             ~url:Mal_url.search_character ~ttl:app.cfg.cache_search_expire);
          Dream.get "/characters/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.character id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_character ~encode:Types.character_to_yojson
                  in
                  respond r));
          Dream.get "/characters/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    App.fetch app ~url:(Mal_url.character id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_character ~encode:(fun c ->
                        Json.data
                          (match Types.character_to_yojson c with
                          | `Assoc fields ->
                              `Assoc
                                (fields
                                @ [
                                    ("anime", `List []);
                                    ("manga", `List []);
                                    ("voices", `List []);
                                  ])
                          | json -> json))
                  in
                  respond r));
          Dream.get "/characters/:id/anime" (fun req ->
              sub_data app req Mal_url.character app.cfg.cache_default_expire
                Parse.parse_character_anime (fun xs ->
                    Json.list
                      (fun u ->
                        `Assoc
                          [
                            ("role", `String "Main");
                            ("anime", mal_url_entry u);
                          ])
                      xs));
          Dream.get "/characters/:id/manga" (fun req ->
              sub_data app req Mal_url.character app.cfg.cache_default_expire
                Parse.parse_character_manga (fun xs ->
                    Json.list
                      (fun u ->
                        `Assoc
                          [
                            ("role", `String "Main");
                            ("manga", mal_url_entry u);
                          ])
                      xs));
          Dream.get "/characters/:id/voices" (fun req ->
              sub_data app req Mal_url.character app.cfg.cache_default_expire
                Parse.parse_character_voices (fun xs ->
                    Json.list Types.person_mini_to_yojson xs));
          Dream.get "/characters/:id/pictures" (fun req ->
              sub_data app req Mal_url.character_pictures
                app.cfg.cache_default_expire Parse.parse_pictures (fun xs ->
                    Json.list Types.picture_to_yojson xs));
          Dream.get "/people" (search app ~kind:"people" ~url:Mal_url.search_people
             ~ttl:app.cfg.cache_search_expire);
          Dream.get "/people/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.people id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_person ~encode:Types.person_to_yojson
                  in
                  respond r));
          Dream.get "/people/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.people id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_person ~encode:Types.person_to_yojson
                  in
                  respond r));
          Dream.get "/people/:id/anime" (fun req ->
              sub_data app req Mal_url.people app.cfg.cache_default_expire
                Parse.parse_character_anime (fun xs ->
                    Json.list mal_url_entry xs));
          Dream.get "/people/:id/manga" (fun req ->
              sub_data app req Mal_url.people app.cfg.cache_default_expire
                Parse.parse_character_manga (fun xs ->
                    Json.list mal_url_entry xs));
          Dream.get "/people/:id/voices" (fun req ->
              sub_data app req Mal_url.people app.cfg.cache_default_expire
                Parse.parse_character_voices (fun xs ->
                    Json.list Types.person_to_yojson xs));
          Dream.get "/people/:id/pictures" (fun req ->
              sub_data app req Mal_url.people_pictures app.cfg.cache_default_expire
                Parse.parse_pictures (fun xs -> Json.list Types.picture_to_yojson xs));
          Dream.get "/seasons" (fun _req ->
              let%lwt r =
                fetch_data app ~url:Mal_url.season_archive
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal ~encode:(fun xs ->
                      Json.list mal_url_entry xs)
              in
              respond r);
          Dream.get "/seasons/now" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.season_now
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/seasons/upcoming" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.season_upcoming
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/seasons/:year/:season" (fun req ->
              let page = page_of req in
              match int_of_string_opt (Dream.param req "year") with
              | None -> Error.respond (Error.Bad_request "invalid year")
              | Some year ->
                  let season = String.lowercase_ascii (Dream.param req "season") in
                  let%lwt r =
                    App.fetch app ~url:(Mal_url.season year season)
                      ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
                  in
                  respond r);
          Dream.get "/schedules" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.schedule ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/schedules/:filter" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.schedule ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/producers" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.producers
                  ~ttl:app.cfg.cache_producers_expire ~parse:Parse.parse_producers ~encode:(fun xs -> wrap_list page 25 xs Types.producer_to_yojson)
              in
              respond r);
          Dream.get "/producers/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.producer id)
                      ~ttl:app.cfg.cache_producers_expire ~parse:Parse.parse_producer ~encode:Types.producer_to_yojson
                  in
                  respond r));
          Dream.get "/producers/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.producer id)
                      ~ttl:app.cfg.cache_producers_expire ~parse:Parse.parse_producer ~encode:Types.producer_to_yojson
                  in
                  respond r));
          Dream.get "/producers/:id/external" (fun req ->
              sub_data app req Mal_url.producer app.cfg.cache_producers_expire
                Parse.parse_external (fun xs ->
                    Json.list Types.external_link_to_yojson xs));
          Dream.get "/magazines" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.magazines
                  ~ttl:app.cfg.cache_magazines_expire ~parse:Parse.parse_magazines ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/users" (search app ~kind:"profile" ~url:(fun q page ->
              ignore page;
              Mal_url.search_users q)
             ~ttl:app.cfg.cache_search_expire);
          Dream.get "/users/recentlyonline" (fun _req ->
              json_ok (Json.data (`List [])));
          Dream.get "/users/userbyid/:id" (fun req ->
              json_ok
                (Json.data
                   (`Assoc
                      [
                        ("mal_id", `String (Dream.param req "id"));
                        ("username", `Null);
                      ])));
          Dream.get "/users/:username" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user username)
                  ~ttl:app.cfg.cache_user_expire
                  ~parse:(fun html -> Parse.parse_user html username)
                  ~encode:(fun u -> Json.data (Types.user_profile_to_yojson u))
              in
              respond r);
          Dream.get "/users/:username/full" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user username)
                  ~ttl:app.cfg.cache_user_expire
                  ~parse:(fun html -> Parse.parse_user html username)
                  ~encode:(fun u -> Json.data (Types.user_profile_to_yojson u))
              in
              respond r);
          Dream.get "/users/:username/statistics" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                fetch_data app ~url:(Mal_url.user_stats username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_user_stats ~encode:Types.user_stats_to_yojson
              in
              respond r);
          Dream.get "/users/:username/favorites" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user_favorites username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_seasonal ~encode:(fun xs ->
                    Json.data
                      (`Assoc
                         [
                           ("anime", Json.list mal_url_entry xs);
                           ("manga", `List []);
                           ("characters", `List []);
                           ("people", `List []);
                         ]))
              in
              respond r);
          Dream.get "/users/:username/userupdates" (fun _req ->
              json_ok
                (Json.data (`Assoc [ ("anime", `List []); ("manga", `List []) ])));
          Dream.get "/users/:username/about" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_moreinfo ~encode:(fun s -> Json.data (`Assoc [ ("about", Json.opt_string s) ]))
              in
              respond r);
          Dream.get "/users/:username/history" (fun _req ->
              json_ok (Json.data (`List [])));
          Dream.get "/users/:username/history/:type" (fun _req ->
              json_ok (Json.data (`List [])));
          Dream.get "/users/:username/friends" (fun req ->
              let username = Dream.param req "username" in
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user_friends username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/users/:username/animelist" (fun req ->
              if app.cfg.disable_user_lists then
                Error.respond (Error.Bad_request "User lists are disabled")
              else
                let username = Dream.param req "username" in
                let page = page_of req in
                let%lwt r =
                  App.fetch app ~url:(Mal_url.user_animelist username)
                    ~ttl:app.cfg.cache_userlist_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
                in
                respond r);
          Dream.get "/users/:username/animelist/:status" (fun req ->
              if app.cfg.disable_user_lists then
                Error.respond (Error.Bad_request "User lists are disabled")
              else
                let username = Dream.param req "username" in
                let page = page_of req in
                let%lwt r =
                  App.fetch app ~url:(Mal_url.user_animelist username)
                    ~ttl:app.cfg.cache_userlist_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
                in
                respond r);
          Dream.get "/users/:username/mangalist" (fun req ->
              if app.cfg.disable_user_lists then
                Error.respond (Error.Bad_request "User lists are disabled")
              else
                let username = Dream.param req "username" in
                let page = page_of req in
                let%lwt r =
                  App.fetch app ~url:(Mal_url.user_mangalist username)
                    ~ttl:app.cfg.cache_userlist_expire
                    ~parse:(Parse.parse_search "manga")
                    ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
                in
                respond r);
          Dream.get "/users/:username/mangalist/:status" (fun _req ->
              if app.cfg.disable_user_lists then
                Error.respond (Error.Bad_request "User lists are disabled")
              else json_ok (wrap_list 1 25 [] mal_url_entry));
          Dream.get "/users/:username/recommendations" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user_recs username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_recommendations ~encode:(fun xs -> Json.data (Json.list Types.recommendation_to_yojson xs))
              in
              respond r);
          Dream.get "/users/:username/reviews" (fun req ->
              let username = Dream.param req "username" in
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user_reviews username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_reviews ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)
              in
              respond r);
          Dream.get "/users/:username/clubs" (fun req ->
              let username = Dream.param req "username" in
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:(Mal_url.user_clubs username)
                  ~ttl:app.cfg.cache_user_expire ~parse:(Parse.parse_search "club") ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/users/:username/external" (fun req ->
              let username = Dream.param req "username" in
              let%lwt r =
                fetch_data app ~url:(Mal_url.user username)
                  ~ttl:app.cfg.cache_user_expire ~parse:Parse.parse_external ~encode:(fun xs -> Json.list Types.external_link_to_yojson xs)
              in
              respond r);
          Dream.get "/genres/anime" (fun _req ->
              let%lwt r =
                fetch_data app ~url:Mal_url.genres_anime
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_genres ~encode:(fun xs -> Json.list mal_url_entry xs)
              in
              respond r);
          Dream.get "/genres/manga" (fun _req ->
              let%lwt r =
                fetch_data app ~url:Mal_url.genres_manga
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_genres ~encode:(fun xs -> Json.list mal_url_entry xs)
              in
              respond r);
          Dream.get "/top/anime" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app
                  ~url:(Mal_url.top_anime ^ "?limit=25&page=" ^ string_of_int page)
                  ~ttl:app.cfg.cache_meta_expire ~parse:(Parse.parse_top "anime")
                  ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/top/manga" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.top_manga ~ttl:app.cfg.cache_meta_expire
                  ~parse:(Parse.parse_top "manga") ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/top/characters" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.top_characters
                  ~ttl:app.cfg.cache_meta_expire ~parse:(Parse.parse_top "character")
                  ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/top/people" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.top_people ~ttl:app.cfg.cache_meta_expire
                  ~parse:(Parse.parse_top "people") ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
              in
              respond r);
          Dream.get "/top/reviews" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.top_reviews ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_reviews ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)
              in
              respond r);
          Dream.get "/clubs" (search app ~kind:"club" ~url:(fun q page ->
              ignore page;
              Mal_url.search_clubs q)
             ~ttl:app.cfg.cache_search_expire);
          Dream.get "/clubs/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r =
                    fetch_data app ~url:(Mal_url.club id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_club ~encode:Types.club_to_yojson
                  in
                  respond r));
          Dream.get "/clubs/:id/members" (fun req ->
              with_id req (fun id ->
                  let page = page_of req in
                  let%lwt r =
                    App.fetch app ~url:(Mal_url.club id)
                      ~ttl:app.cfg.cache_default_expire ~parse:Parse.parse_seasonal ~encode:(fun xs -> wrap_list page 25 xs mal_url_entry)
                  in
                  respond r));
          Dream.get "/clubs/:id/staff" (fun req ->
              sub_data app req Mal_url.club app.cfg.cache_default_expire
                Parse.parse_anime_staff (fun xs ->
                    Json.list Types.staff_entry_to_yojson xs));
          Dream.get "/clubs/:id/relations" (fun req ->
              sub_data app req Mal_url.club app.cfg.cache_default_expire
                Parse.parse_relations (fun xs -> Json.list Types.relation_to_yojson xs));
          Dream.get "/reviews/anime" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.reviews_anime
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_reviews ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)
              in
              respond r);
          Dream.get "/reviews/manga" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.reviews_manga
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_reviews ~encode:(fun xs -> wrap_list page 25 xs Types.review_to_yojson)
              in
              respond r);
          Dream.get "/recommendations/anime" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.recs_anime ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_recommendations ~encode:(fun xs ->
                      wrap_list page 25 xs Types.recommendation_to_yojson)
              in
              respond r);
          Dream.get "/recommendations/manga" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.recs_manga ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_recommendations ~encode:(fun xs ->
                      wrap_list page 25 xs Types.recommendation_to_yojson)
              in
              respond r);
          Dream.get "/watch/episodes" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.watch_episodes
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_watch ~encode:(fun xs -> wrap_list page 25 xs Types.watch_episode_to_yojson)
              in
              respond r);
          Dream.get "/watch/episodes/popular" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:(Mal_url.watch_episodes ^ "?popular")
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_watch ~encode:(fun xs -> wrap_list page 25 xs Types.watch_episode_to_yojson)
              in
              respond r);
          Dream.get "/watch/promos" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:Mal_url.watch_promos ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_videos ~encode:(fun v ->
                      wrap_list page 25 v.Types.promo Types.promo_video_to_yojson)
              in
              respond r);
          Dream.get "/watch/promos/popular" (fun req ->
              let page = page_of req in
              let%lwt r =
                App.fetch app ~url:(Mal_url.watch_promos ^ "?popular")
                  ~ttl:app.cfg.cache_meta_expire ~parse:Parse.parse_videos ~encode:(fun v ->
                      wrap_list page 25 v.Types.promo Types.promo_video_to_yojson)
              in
              respond r);
          Dream.get "/random/anime" (fun _req ->
              let%lwt r =
                App.fetch app ~url:Mal_url.season_now ~ttl:60 ~parse:Parse.parse_seasonal ~encode:(fun xs ->
                    match xs with
                    | [] -> Json.data (`Assoc [])
                    | _ ->
                        let i = Random.int (List.length xs) in
                        Json.data (mal_url_entry (List.nth xs i)))
              in
              respond r);
          Dream.get "/random/manga" (fun _req ->
              let%lwt r =
                App.fetch app ~url:Mal_url.top_manga ~ttl:60 ~parse:(Parse.parse_top "manga")
                  ~encode:(fun xs ->
                    match xs with
                    | [] -> Json.data (`Assoc [])
                    | _ ->
                        let i = Random.int (List.length xs) in
                        Json.data (mal_url_entry (List.nth xs i)))
              in
              respond r);
          Dream.get "/random/characters" (fun _req ->
              json_ok (Json.data (`Assoc [])));
          Dream.get "/random/people" (fun _req -> json_ok (Json.data (`Assoc [])));
          Dream.get "/random/users" (fun _req -> json_ok (Json.data (`Assoc [])));
        ];
    ]
