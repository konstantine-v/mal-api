type t = {
  name : string;
  description : string;
  input_schema : Yojson.Safe.t;
  run : App.t -> Yojson.Safe.t -> (Yojson.Safe.t, Error.t) result Lwt.t;
}

let fields = function `Assoc fs -> fs | _ -> []

let get_field name json = List.assoc_opt name (fields json)

let as_int name = function
  | `Int n -> Ok n
  | `Float f when float_of_int (int_of_float f) = f -> Ok (int_of_float f)
  | `String s -> (
      match int_of_string_opt (String.trim s) with
      | Some n -> Ok n
      | None -> Error (Error.Bad_request (name ^ " must be an integer")))
  | _ -> Error (Error.Bad_request (name ^ " must be an integer"))

let req_int name json =
  match get_field name json with
  | None | Some `Null -> Error (Error.Bad_request (name ^ " is required"))
  | Some v -> as_int name v

let opt_page json =
  match get_field "page" json with
  | None | Some `Null -> Ok 1
  | Some v -> (
      match as_int "page" v with
      | Ok n -> Ok (max 1 n)
      | Error e -> Error e)

let req_string name json =
  match get_field name json with
  | Some (`String s) when String.trim s <> "" -> Ok s
  | Some (`Int n) -> Ok (string_of_int n)
  | _ -> Error (Error.Bad_request (name ^ " is required"))

let req_id json =
  match req_int "id" json with
  | Error e -> Error e
  | Ok n -> (
      match Mal_id.of_int n with
      | Some id -> Ok id
      | None -> Error (Error.Bad_request "id must be a positive integer"))

let ok_sync json = Lwt.return (Ok json)

let fail e = Lwt.return (Error e)

let bind r f = match r with Error e -> fail e | Ok x -> f x

let object_schema ~required properties =
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List (List.map (fun s -> `String s) required));
      ("additionalProperties", `Bool false);
    ]

let empty_schema = object_schema ~required:[] []

let int_prop desc =
  `Assoc [ ("type", `String "integer"); ("minimum", `Int 1); ("description", `String desc) ]

let string_prop desc = `Assoc [ ("type", `String "string"); ("description", `String desc) ]

let page_prop =
  `Assoc
    [
      ("type", `String "integer");
      ("minimum", `Int 1);
      ("default", `Int 1);
      ("description", `String "Page number");
    ]

let tool name description input_schema run = { name; description; input_schema; run }

let no_args name description run =
  tool name description empty_schema (fun app _args -> run app)

let id_tool name description run =
  tool name description
    (object_schema ~required:[ "id" ] [ ("id", int_prop "MyAnimeList id") ])
    (fun app args -> bind (req_id args) (fun id -> run app id))

let id_page_tool name description run =
  tool name description
    (object_schema ~required:[ "id" ]
       [ ("id", int_prop "MyAnimeList id"); ("page", page_prop) ])
    (fun app args ->
      bind (req_id args) (fun id -> bind (opt_page args) (fun page -> run app id ~page)))

let search_tool name description run =
  tool name description
    (object_schema ~required:[ "q" ]
       [ ("q", string_prop "Search query"); ("page", page_prop) ])
    (fun app args ->
      bind (req_string "q" args) (fun q -> bind (opt_page args) (fun page -> run app ~q ~page)))

let page_tool name description run =
  tool name description (object_schema ~required:[] [ ("page", page_prop) ]) (fun app args ->
      bind (opt_page args) (fun page -> run app ~page))

let username_tool name description run =
  tool name description
    (object_schema ~required:[ "username" ] [ ("username", string_prop "MAL username") ])
    (fun app args -> bind (req_string "username" args) (fun username -> run app username))

let username_page_tool name description run =
  tool name description
    (object_schema ~required:[ "username" ]
       [ ("username", string_prop "MAL username"); ("page", page_prop) ])
    (fun app args ->
      bind (req_string "username" args) (fun username ->
          bind (opt_page args) (fun page -> run app username ~page)))

let all : t list =
  [
    no_args "get_root" "API metadata and MyAnimeList heartbeat (GET /)" (fun app ->
        ok_sync (Ops.root app));
    search_tool "search_anime" "Search anime (GET /v4/anime)" Ops.search_anime;
    id_tool "get_anime" "Get anime by id (GET /v4/anime/{id})" Ops.get_anime;
    id_tool "get_anime_full" "Get full anime (GET /v4/anime/{id}/full)" Ops.get_anime_full;
    id_tool "get_anime_characters" "Anime characters (GET /v4/anime/{id}/characters)"
      Ops.get_anime_characters;
    id_tool "get_anime_staff" "Anime staff (GET /v4/anime/{id}/staff)" Ops.get_anime_staff;
    id_page_tool "get_anime_episodes" "Anime episodes (GET /v4/anime/{id}/episodes)"
      Ops.get_anime_episodes;
    tool "get_anime_episode" "Single episode (GET /v4/anime/{id}/episodes/{episodeId})"
      (object_schema ~required:[ "id"; "episodeId" ]
         [ ("id", int_prop "Anime id"); ("episodeId", int_prop "Episode number") ])
      (fun app args ->
        bind (req_id args) (fun id ->
            bind (req_int "episodeId" args) (fun ep ->
                if ep <= 0 then fail (Error.Bad_request "invalid episode id")
                else Ops.get_anime_episode app id ep)));
    id_page_tool "get_anime_news" "Anime news (GET /v4/anime/{id}/news)" Ops.get_anime_news;
    id_tool "get_anime_forum" "Anime forum (GET /v4/anime/{id}/forum)" Ops.get_anime_forum;
    id_tool "get_anime_videos" "Anime videos (GET /v4/anime/{id}/videos)" Ops.get_anime_videos;
    id_tool "get_anime_videos_episodes" "Anime video episodes (GET /v4/anime/{id}/videos/episodes)"
      Ops.get_anime_video_episodes;
    id_tool "get_anime_pictures" "Anime pictures (GET /v4/anime/{id}/pictures)" Ops.get_anime_pictures;
    id_tool "get_anime_statistics" "Anime statistics (GET /v4/anime/{id}/statistics)"
      Ops.get_anime_statistics;
    id_tool "get_anime_moreinfo" "Anime moreinfo (GET /v4/anime/{id}/moreinfo)" Ops.get_anime_moreinfo;
    id_tool "get_anime_recommendations" "Anime recommendations (GET /v4/anime/{id}/recommendations)"
      Ops.get_anime_recommendations;
    id_tool "get_anime_userupdates" "Anime user updates (GET /v4/anime/{id}/userupdates)"
      Ops.get_anime_userupdates;
    id_page_tool "get_anime_reviews" "Anime reviews (GET /v4/anime/{id}/reviews)" Ops.get_anime_reviews;
    id_tool "get_anime_relations" "Anime relations (GET /v4/anime/{id}/relations)"
      Ops.get_anime_relations;
    id_tool "get_anime_themes" "Anime themes (GET /v4/anime/{id}/themes)" Ops.get_anime_themes;
    id_tool "get_anime_external" "Anime external links (GET /v4/anime/{id}/external)"
      Ops.get_anime_external;
    id_tool "get_anime_streaming" "Anime streaming (GET /v4/anime/{id}/streaming)"
      Ops.get_anime_streaming;
    search_tool "search_manga" "Search manga (GET /v4/manga)" Ops.search_manga;
    id_tool "get_manga" "Get manga by id (GET /v4/manga/{id})" Ops.get_manga;
    id_tool "get_manga_full" "Get full manga (GET /v4/manga/{id}/full)" Ops.get_manga_full;
    id_tool "get_manga_characters" "Manga characters (GET /v4/manga/{id}/characters)"
      Ops.get_manga_characters;
    id_page_tool "get_manga_news" "Manga news (GET /v4/manga/{id}/news)" Ops.get_manga_news;
    id_tool "get_manga_forum" "Manga forum (GET /v4/manga/{id}/forum)" Ops.get_manga_forum;
    id_tool "get_manga_pictures" "Manga pictures (GET /v4/manga/{id}/pictures)" Ops.get_manga_pictures;
    id_tool "get_manga_statistics" "Manga statistics (GET /v4/manga/{id}/statistics)"
      Ops.get_manga_statistics;
    id_tool "get_manga_moreinfo" "Manga moreinfo (GET /v4/manga/{id}/moreinfo)" Ops.get_manga_moreinfo;
    id_tool "get_manga_recommendations" "Manga recommendations (GET /v4/manga/{id}/recommendations)"
      Ops.get_manga_recommendations;
    id_tool "get_manga_userupdates" "Manga user updates (GET /v4/manga/{id}/userupdates)"
      Ops.get_manga_userupdates;
    id_page_tool "get_manga_reviews" "Manga reviews (GET /v4/manga/{id}/reviews)" Ops.get_manga_reviews;
    id_tool "get_manga_relations" "Manga relations (GET /v4/manga/{id}/relations)"
      Ops.get_manga_relations;
    id_tool "get_manga_external" "Manga external links (GET /v4/manga/{id}/external)"
      Ops.get_manga_external;
    search_tool "search_characters" "Search characters (GET /v4/characters)" Ops.search_characters;
    id_tool "get_character" "Get character (GET /v4/characters/{id})" Ops.get_character;
    id_tool "get_character_full" "Get full character (GET /v4/characters/{id}/full)"
      Ops.get_character_full;
    id_tool "get_character_anime" "Character anime (GET /v4/characters/{id}/anime)"
      Ops.get_character_anime;
    id_tool "get_character_manga" "Character manga (GET /v4/characters/{id}/manga)"
      Ops.get_character_manga;
    id_tool "get_character_voices" "Character voices (GET /v4/characters/{id}/voices)"
      Ops.get_character_voices;
    id_tool "get_character_pictures" "Character pictures (GET /v4/characters/{id}/pictures)"
      Ops.get_character_pictures;
    search_tool "search_people" "Search people (GET /v4/people)" Ops.search_people;
    id_tool "get_person" "Get person (GET /v4/people/{id})" Ops.get_person;
    id_tool "get_person_full" "Get full person (GET /v4/people/{id}/full)" Ops.get_person_full;
    id_tool "get_person_anime" "Person anime (GET /v4/people/{id}/anime)" Ops.get_person_anime;
    id_tool "get_person_manga" "Person manga (GET /v4/people/{id}/manga)" Ops.get_person_manga;
    id_tool "get_person_voices" "Person voices (GET /v4/people/{id}/voices)" Ops.get_person_voices;
    id_tool "get_person_pictures" "Person pictures (GET /v4/people/{id}/pictures)"
      Ops.get_person_pictures;
    no_args "get_seasons" "Season archive (GET /v4/seasons)" Ops.get_seasons;
    page_tool "get_seasons_now" "Current season (GET /v4/seasons/now)" Ops.get_seasons_now;
    page_tool "get_seasons_upcoming" "Upcoming season (GET /v4/seasons/upcoming)"
      Ops.get_seasons_upcoming;
    tool "get_season" "Season by year (GET /v4/seasons/{year}/{season})"
      (object_schema ~required:[ "year"; "season" ]
         [
           ("year", int_prop "Year");
           ("season", string_prop "winter, spring, summer, or fall");
           ("page", page_prop);
         ])
      (fun app args ->
        bind (req_int "year" args) (fun year ->
            bind (req_string "season" args) (fun season ->
                bind (opt_page args) (fun page ->
                    Ops.get_season app ~year ~season:(String.lowercase_ascii season) ~page))));
    page_tool "get_schedules" "Weekly schedule (GET /v4/schedules)" Ops.get_schedules;
    tool "get_schedules_filter" "Schedule by day filter (GET /v4/schedules/{filter})"
      (object_schema ~required:[ "filter" ]
         [ ("filter", string_prop "Day filter"); ("page", page_prop) ])
      (fun app args ->
        bind (req_string "filter" args) (fun filter ->
            bind (opt_page args) (fun page -> Ops.get_schedules_filter app ~filter ~page)));
    page_tool "get_producers" "Producers list (GET /v4/producers)" Ops.get_producers;
    id_tool "get_producer" "Producer (GET /v4/producers/{id})" Ops.get_producer;
    id_tool "get_producer_full" "Full producer (GET /v4/producers/{id}/full)" Ops.get_producer_full;
    id_tool "get_producer_external" "Producer external (GET /v4/producers/{id}/external)"
      Ops.get_producer_external;
    page_tool "get_magazines" "Magazines (GET /v4/magazines)" Ops.get_magazines;
    search_tool "search_users" "Search users (GET /v4/users)" Ops.search_users;
    no_args "get_users_recentlyonline" "Recently online users (GET /v4/users/recentlyonline)"
      (fun _app -> ok_sync (Ops.get_users_recentlyonline ()));
    id_tool "get_users_userbyid" "User by MAL id (GET /v4/users/userbyid/{id})" (fun _app id ->
        ok_sync (Ops.get_user_by_id (string_of_int (Mal_id.to_int id))));
    username_tool "get_user" "User profile (GET /v4/users/{username})" Ops.get_user;
    username_tool "get_user_full" "Full user profile (GET /v4/users/{username}/full)" Ops.get_user_full;
    username_tool "get_user_statistics" "User statistics (GET /v4/users/{username}/statistics)"
      Ops.get_user_statistics;
    username_tool "get_user_favorites" "User favorites (GET /v4/users/{username}/favorites)"
      Ops.get_user_favorites;
    username_tool "get_user_userupdates" "User updates (GET /v4/users/{username}/userupdates)"
      (fun _app _username -> ok_sync (Ops.get_user_userupdates ()));
    username_tool "get_user_about" "User about (GET /v4/users/{username}/about)" Ops.get_user_about;
    username_tool "get_user_history" "User history (GET /v4/users/{username}/history)" (fun _app _u ->
        ok_sync (Ops.get_user_history ()));
    tool "get_user_history_type" "User history by type (GET /v4/users/{username}/history/{type})"
      (object_schema ~required:[ "username"; "type" ]
         [
           ("username", string_prop "MAL username");
           ("type", string_prop "anime or manga");
         ])
      (fun _app args ->
        bind (req_string "username" args) (fun _username ->
            bind (req_string "type" args) (fun history_type ->
                ok_sync (Ops.get_user_history_type ~history_type))));
    username_page_tool "get_user_friends" "User friends (GET /v4/users/{username}/friends)"
      Ops.get_user_friends;
    username_page_tool "get_user_animelist" "User animelist (GET /v4/users/{username}/animelist)"
      Ops.get_user_animelist;
    tool "get_user_animelist_status"
      "User animelist by status (GET /v4/users/{username}/animelist/{status})"
      (object_schema ~required:[ "username"; "status" ]
         [
           ("username", string_prop "MAL username");
           ("status", string_prop "List status");
           ("page", page_prop);
         ])
      (fun app args ->
        bind (req_string "username" args) (fun username ->
            bind (req_string "status" args) (fun status ->
                bind (opt_page args) (fun page ->
                    Ops.get_user_animelist_status app username ~status ~page))));
    username_page_tool "get_user_mangalist" "User mangalist (GET /v4/users/{username}/mangalist)"
      Ops.get_user_mangalist;
    tool "get_user_mangalist_status"
      "User mangalist by status (GET /v4/users/{username}/mangalist/{status})"
      (object_schema ~required:[ "username"; "status" ]
         [
           ("username", string_prop "MAL username");
           ("status", string_prop "List status");
         ])
      (fun app args ->
        bind (req_string "username" args) (fun username ->
            bind (req_string "status" args) (fun status ->
                Ops.get_user_mangalist_status app username ~status)));
    username_tool "get_user_recommendations"
      "User recommendations (GET /v4/users/{username}/recommendations)" Ops.get_user_recommendations;
    username_page_tool "get_user_reviews" "User reviews (GET /v4/users/{username}/reviews)"
      Ops.get_user_reviews;
    username_page_tool "get_user_clubs" "User clubs (GET /v4/users/{username}/clubs)" Ops.get_user_clubs;
    username_tool "get_user_external" "User external (GET /v4/users/{username}/external)"
      Ops.get_user_external;
    no_args "get_genres_anime" "Anime genres (GET /v4/genres/anime)" Ops.get_genres_anime;
    no_args "get_genres_manga" "Manga genres (GET /v4/genres/manga)" Ops.get_genres_manga;
    page_tool "get_top_anime" "Top anime (GET /v4/top/anime)" Ops.get_top_anime;
    page_tool "get_top_manga" "Top manga (GET /v4/top/manga)" Ops.get_top_manga;
    page_tool "get_top_characters" "Top characters (GET /v4/top/characters)" Ops.get_top_characters;
    page_tool "get_top_people" "Top people (GET /v4/top/people)" Ops.get_top_people;
    page_tool "get_top_reviews" "Top reviews (GET /v4/top/reviews)" Ops.get_top_reviews;
    search_tool "search_clubs" "Search clubs (GET /v4/clubs)" Ops.search_clubs;
    id_tool "get_club" "Club (GET /v4/clubs/{id})" Ops.get_club;
    id_page_tool "get_club_members" "Club members (GET /v4/clubs/{id}/members)" Ops.get_club_members;
    id_tool "get_club_staff" "Club staff (GET /v4/clubs/{id}/staff)" Ops.get_club_staff;
    id_tool "get_club_relations" "Club relations (GET /v4/clubs/{id}/relations)" Ops.get_club_relations;
    page_tool "get_reviews_anime" "Recent anime reviews (GET /v4/reviews/anime)" Ops.get_reviews_anime;
    page_tool "get_reviews_manga" "Recent manga reviews (GET /v4/reviews/manga)" Ops.get_reviews_manga;
    page_tool "get_recommendations_anime" "Recent anime recommendations (GET /v4/recommendations/anime)"
      Ops.get_recommendations_anime;
    page_tool "get_recommendations_manga" "Recent manga recommendations (GET /v4/recommendations/manga)"
      Ops.get_recommendations_manga;
    page_tool "get_watch_episodes" "Watch episodes (GET /v4/watch/episodes)" Ops.get_watch_episodes;
    page_tool "get_watch_episodes_popular" "Popular watch episodes (GET /v4/watch/episodes/popular)"
      Ops.get_watch_episodes_popular;
    page_tool "get_watch_promos" "Watch promos (GET /v4/watch/promos)" Ops.get_watch_promos;
    page_tool "get_watch_promos_popular" "Popular watch promos (GET /v4/watch/promos/popular)"
      Ops.get_watch_promos_popular;
    no_args "get_random_anime" "Random anime (GET /v4/random/anime)" Ops.get_random_anime;
    no_args "get_random_manga" "Random manga (GET /v4/random/manga)" Ops.get_random_manga;
    no_args "get_random_characters" "Random character (GET /v4/random/characters)" (fun _app ->
        ok_sync (Ops.get_random_characters ()));
    no_args "get_random_people" "Random person (GET /v4/random/people)" (fun _app ->
        ok_sync (Ops.get_random_people ()));
    no_args "get_random_users" "Random user (GET /v4/random/users)" (fun _app ->
        ok_sync (Ops.get_random_users ()));
  ]

let by_name =
  let tbl = Hashtbl.create (List.length all) in
  List.iter (fun t -> Hashtbl.replace tbl t.name t) all;
  tbl

let find name = Hashtbl.find_opt by_name name
