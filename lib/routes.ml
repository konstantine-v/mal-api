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

let with_id req f =
  match id_of req with Error e -> Error.respond e | Ok id -> f id

let handler app =
  Dream.router
    [
      Dream.get "/" (fun _req -> json_ok (Ops.root app));
      Dream.post "/mcp" (Mcp.post app);
      Dream.get "/mcp" Mcp.get;
      Dream.delete "/mcp" Mcp.delete;
      Dream.scope "/v4" []
        [
          Dream.get "/anime" (fun req ->
              let%lwt r = Ops.search_anime app ~q:(q_of req) ~page:(page_of req) in
              respond r);
          Dream.get "/anime/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime app id in
                  respond r));
          Dream.get "/anime/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_full app id in
                  respond r));
          Dream.get "/anime/:id/characters" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_characters app id in
                  respond r));
          Dream.get "/anime/:id/staff" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_staff app id in
                  respond r));
          Dream.get "/anime/:id/episodes" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_episodes app id ~page:(page_of req) in
                  respond r));
          Dream.get "/anime/:id/episodes/:episodeId" (fun req ->
              with_id req (fun id ->
                  match int_of_string_opt (Dream.param req "episodeId") with
                  | None -> Error.respond (Error.Bad_request "invalid episode id")
                  | Some ep ->
                      let%lwt r = Ops.get_anime_episode app id ep in
                      respond r));
          Dream.get "/anime/:id/news" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_news app id ~page:(page_of req) in
                  respond r));
          Dream.get "/anime/:id/forum" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_forum app id in
                  respond r));
          Dream.get "/anime/:id/videos" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_videos app id in
                  respond r));
          Dream.get "/anime/:id/videos/episodes" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_video_episodes app id in
                  respond r));
          Dream.get "/anime/:id/pictures" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_pictures app id in
                  respond r));
          Dream.get "/anime/:id/statistics" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_statistics app id in
                  respond r));
          Dream.get "/anime/:id/moreinfo" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_moreinfo app id in
                  respond r));
          Dream.get "/anime/:id/recommendations" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_recommendations app id in
                  respond r));
          Dream.get "/anime/:id/userupdates" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_userupdates app id in
                  respond r));
          Dream.get "/anime/:id/reviews" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_reviews app id ~page:(page_of req) in
                  respond r));
          Dream.get "/anime/:id/relations" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_relations app id in
                  respond r));
          Dream.get "/anime/:id/themes" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_themes app id in
                  respond r));
          Dream.get "/anime/:id/external" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_external app id in
                  respond r));
          Dream.get "/anime/:id/streaming" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_anime_streaming app id in
                  respond r));
          Dream.get "/manga" (fun req ->
              let%lwt r = Ops.search_manga app ~q:(q_of req) ~page:(page_of req) in
              respond r);
          Dream.get "/manga/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga app id in
                  respond r));
          Dream.get "/manga/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_full app id in
                  respond r));
          Dream.get "/manga/:id/characters" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_characters app id in
                  respond r));
          Dream.get "/manga/:id/news" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_news app id ~page:(page_of req) in
                  respond r));
          Dream.get "/manga/:id/forum" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_forum app id in
                  respond r));
          Dream.get "/manga/:id/pictures" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_pictures app id in
                  respond r));
          Dream.get "/manga/:id/statistics" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_statistics app id in
                  respond r));
          Dream.get "/manga/:id/moreinfo" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_moreinfo app id in
                  respond r));
          Dream.get "/manga/:id/recommendations" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_recommendations app id in
                  respond r));
          Dream.get "/manga/:id/userupdates" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_userupdates app id in
                  respond r));
          Dream.get "/manga/:id/reviews" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_reviews app id ~page:(page_of req) in
                  respond r));
          Dream.get "/manga/:id/relations" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_relations app id in
                  respond r));
          Dream.get "/manga/:id/external" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_manga_external app id in
                  respond r));
          Dream.get "/characters" (fun req ->
              let%lwt r = Ops.search_characters app ~q:(q_of req) ~page:(page_of req) in
              respond r);
          Dream.get "/characters/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_character app id in
                  respond r));
          Dream.get "/characters/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_character_full app id in
                  respond r));
          Dream.get "/characters/:id/anime" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_character_anime app id in
                  respond r));
          Dream.get "/characters/:id/manga" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_character_manga app id in
                  respond r));
          Dream.get "/characters/:id/voices" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_character_voices app id in
                  respond r));
          Dream.get "/characters/:id/pictures" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_character_pictures app id in
                  respond r));
          Dream.get "/people" (fun req ->
              let%lwt r = Ops.search_people app ~q:(q_of req) ~page:(page_of req) in
              respond r);
          Dream.get "/people/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_person app id in
                  respond r));
          Dream.get "/people/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_person_full app id in
                  respond r));
          Dream.get "/people/:id/anime" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_person_anime app id in
                  respond r));
          Dream.get "/people/:id/manga" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_person_manga app id in
                  respond r));
          Dream.get "/people/:id/voices" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_person_voices app id in
                  respond r));
          Dream.get "/people/:id/pictures" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_person_pictures app id in
                  respond r));
          Dream.get "/seasons" (fun _req ->
              let%lwt r = Ops.get_seasons app in
              respond r);
          Dream.get "/seasons/now" (fun req ->
              let%lwt r = Ops.get_seasons_now app ~page:(page_of req) in
              respond r);
          Dream.get "/seasons/upcoming" (fun req ->
              let%lwt r = Ops.get_seasons_upcoming app ~page:(page_of req) in
              respond r);
          Dream.get "/seasons/:year/:season" (fun req ->
              match int_of_string_opt (Dream.param req "year") with
              | None -> Error.respond (Error.Bad_request "invalid year")
              | Some year ->
                  let season = String.lowercase_ascii (Dream.param req "season") in
                  let%lwt r = Ops.get_season app ~year ~season ~page:(page_of req) in
                  respond r);
          Dream.get "/schedules" (fun req ->
              let%lwt r = Ops.get_schedules app ~page:(page_of req) in
              respond r);
          Dream.get "/schedules/:filter" (fun req ->
              let%lwt r =
                Ops.get_schedules_filter app ~filter:(Dream.param req "filter")
                  ~page:(page_of req)
              in
              respond r);
          Dream.get "/producers" (fun req ->
              let%lwt r = Ops.get_producers app ~page:(page_of req) in
              respond r);
          Dream.get "/producers/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_producer app id in
                  respond r));
          Dream.get "/producers/:id/full" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_producer_full app id in
                  respond r));
          Dream.get "/producers/:id/external" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_producer_external app id in
                  respond r));
          Dream.get "/magazines" (fun req ->
              let%lwt r = Ops.get_magazines app ~page:(page_of req) in
              respond r);
          Dream.get "/users" (fun req ->
              let%lwt r = Ops.search_users app ~q:(q_of req) ~page:(page_of req) in
              respond r);
          Dream.get "/users/recentlyonline" (fun _req -> json_ok (Ops.get_users_recentlyonline ()));
          Dream.get "/users/userbyid/:id" (fun req ->
              json_ok (Ops.get_user_by_id (Dream.param req "id")));
          Dream.get "/users/:username" (fun req ->
              let%lwt r = Ops.get_user app (Dream.param req "username") in
              respond r);
          Dream.get "/users/:username/full" (fun req ->
              let%lwt r = Ops.get_user_full app (Dream.param req "username") in
              respond r);
          Dream.get "/users/:username/statistics" (fun req ->
              let%lwt r = Ops.get_user_statistics app (Dream.param req "username") in
              respond r);
          Dream.get "/users/:username/favorites" (fun req ->
              let%lwt r = Ops.get_user_favorites app (Dream.param req "username") in
              respond r);
          Dream.get "/users/:username/userupdates" (fun _req ->
              json_ok (Ops.get_user_userupdates ()));
          Dream.get "/users/:username/about" (fun req ->
              let%lwt r = Ops.get_user_about app (Dream.param req "username") in
              respond r);
          Dream.get "/users/:username/history" (fun _req -> json_ok (Ops.get_user_history ()));
          Dream.get "/users/:username/history/:type" (fun req ->
              json_ok (Ops.get_user_history_type ~history_type:(Dream.param req "type")));
          Dream.get "/users/:username/friends" (fun req ->
              let%lwt r =
                Ops.get_user_friends app (Dream.param req "username") ~page:(page_of req)
              in
              respond r);
          Dream.get "/users/:username/animelist" (fun req ->
              let%lwt r =
                Ops.get_user_animelist app (Dream.param req "username") ~page:(page_of req)
              in
              respond r);
          Dream.get "/users/:username/animelist/:status" (fun req ->
              let%lwt r =
                Ops.get_user_animelist_status app (Dream.param req "username")
                  ~status:(Dream.param req "status") ~page:(page_of req)
              in
              respond r);
          Dream.get "/users/:username/mangalist" (fun req ->
              let%lwt r =
                Ops.get_user_mangalist app (Dream.param req "username") ~page:(page_of req)
              in
              respond r);
          Dream.get "/users/:username/mangalist/:status" (fun req ->
              let%lwt r =
                Ops.get_user_mangalist_status app (Dream.param req "username")
                  ~status:(Dream.param req "status")
              in
              respond r);
          Dream.get "/users/:username/recommendations" (fun req ->
              let%lwt r = Ops.get_user_recommendations app (Dream.param req "username") in
              respond r);
          Dream.get "/users/:username/reviews" (fun req ->
              let%lwt r =
                Ops.get_user_reviews app (Dream.param req "username") ~page:(page_of req)
              in
              respond r);
          Dream.get "/users/:username/clubs" (fun req ->
              let%lwt r =
                Ops.get_user_clubs app (Dream.param req "username") ~page:(page_of req)
              in
              respond r);
          Dream.get "/users/:username/external" (fun req ->
              let%lwt r = Ops.get_user_external app (Dream.param req "username") in
              respond r);
          Dream.get "/genres/anime" (fun _req ->
              let%lwt r = Ops.get_genres_anime app in
              respond r);
          Dream.get "/genres/manga" (fun _req ->
              let%lwt r = Ops.get_genres_manga app in
              respond r);
          Dream.get "/top/anime" (fun req ->
              let%lwt r = Ops.get_top_anime app ~page:(page_of req) in
              respond r);
          Dream.get "/top/manga" (fun req ->
              let%lwt r = Ops.get_top_manga app ~page:(page_of req) in
              respond r);
          Dream.get "/top/characters" (fun req ->
              let%lwt r = Ops.get_top_characters app ~page:(page_of req) in
              respond r);
          Dream.get "/top/people" (fun req ->
              let%lwt r = Ops.get_top_people app ~page:(page_of req) in
              respond r);
          Dream.get "/top/reviews" (fun req ->
              let%lwt r = Ops.get_top_reviews app ~page:(page_of req) in
              respond r);
          Dream.get "/clubs" (fun req ->
              let%lwt r = Ops.search_clubs app ~q:(q_of req) ~page:(page_of req) in
              respond r);
          Dream.get "/clubs/:id" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_club app id in
                  respond r));
          Dream.get "/clubs/:id/members" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_club_members app id ~page:(page_of req) in
                  respond r));
          Dream.get "/clubs/:id/staff" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_club_staff app id in
                  respond r));
          Dream.get "/clubs/:id/relations" (fun req ->
              with_id req (fun id ->
                  let%lwt r = Ops.get_club_relations app id in
                  respond r));
          Dream.get "/reviews/anime" (fun req ->
              let%lwt r = Ops.get_reviews_anime app ~page:(page_of req) in
              respond r);
          Dream.get "/reviews/manga" (fun req ->
              let%lwt r = Ops.get_reviews_manga app ~page:(page_of req) in
              respond r);
          Dream.get "/recommendations/anime" (fun req ->
              let%lwt r = Ops.get_recommendations_anime app ~page:(page_of req) in
              respond r);
          Dream.get "/recommendations/manga" (fun req ->
              let%lwt r = Ops.get_recommendations_manga app ~page:(page_of req) in
              respond r);
          Dream.get "/watch/episodes" (fun req ->
              let%lwt r = Ops.get_watch_episodes app ~page:(page_of req) in
              respond r);
          Dream.get "/watch/episodes/popular" (fun req ->
              let%lwt r = Ops.get_watch_episodes_popular app ~page:(page_of req) in
              respond r);
          Dream.get "/watch/promos" (fun req ->
              let%lwt r = Ops.get_watch_promos app ~page:(page_of req) in
              respond r);
          Dream.get "/watch/promos/popular" (fun req ->
              let%lwt r = Ops.get_watch_promos_popular app ~page:(page_of req) in
              respond r);
          Dream.get "/random/anime" (fun _req ->
              let%lwt r = Ops.get_random_anime app in
              respond r);
          Dream.get "/random/manga" (fun _req ->
              let%lwt r = Ops.get_random_manga app in
              respond r);
          Dream.get "/random/characters" (fun _req -> json_ok (Ops.get_random_characters ()));
          Dream.get "/random/people" (fun _req -> json_ok (Ops.get_random_people ()));
          Dream.get "/random/users" (fun _req -> json_ok (Ops.get_random_users ()));
        ];
    ]
