type images = {
  image_url : string option;
  small_image_url : string option;
  large_image_url : string option;
}

type image_pair = { jpg : images; webp : images }

type trailer_images = {
  image_url : string option;
  small_image_url : string option;
  medium_image_url : string option;
  large_image_url : string option;
  maximum_image_url : string option;
}

type trailer = {
  youtube_id : string option;
  url : string option;
  embed_url : string option;
  images : trailer_images;
}

type title = { type_ : string; title : string }

type date_prop = { day : int option; month : int option; year : int option }

type date_range = {
  from_iso : string option;
  to_iso : string option;
  from_prop : date_prop;
  to_prop : date_prop;
  string_ : string option;
}

type broadcast = {
  day : string option;
  time : string option;
  timezone : string option;
  string_ : string option;
}

type mal_url = {
  mal_id : Mal_id.t;
  type_ : string;
  name : string;
  url : string;
}

type external_link = { name : string; url : string }

type relation = { relation : string; entry : mal_url list }

type anime = {
  mal_id : Mal_id.t;
  url : string;
  images : image_pair;
  trailer : trailer;
  approved : bool;
  titles : title list;
  title : string;
  title_english : string option;
  title_japanese : string option;
  title_synonyms : string list;
  type_ : string option;
  source : string option;
  episodes : int option;
  status : string option;
  airing : bool;
  aired : date_range;
  duration : string option;
  rating : string option;
  score : float option;
  scored_by : int option;
  rank : int option;
  popularity : int option;
  members : int option;
  favorites : int option;
  synopsis : string option;
  background : string option;
  season : string option;
  year : int option;
  broadcast : broadcast;
  producers : mal_url list;
  licensors : mal_url list;
  studios : mal_url list;
  genres : mal_url list;
  explicit_genres : mal_url list;
  themes : mal_url list;
  demographics : mal_url list;
}

type anime_full = {
  anime : anime;
  relations : relation list;
  theme : string list * string list;
  external_links : external_link list;
  streaming : external_link list;
}

type manga = {
  mal_id : Mal_id.t;
  url : string;
  images : image_pair;
  approved : bool;
  titles : title list;
  title : string;
  title_english : string option;
  title_japanese : string option;
  title_synonyms : string list;
  type_ : string option;
  chapters : int option;
  volumes : int option;
  status : string option;
  publishing : bool;
  published : date_range;
  score : float option;
  scored_by : int option;
  rank : int option;
  popularity : int option;
  members : int option;
  favorites : int option;
  synopsis : string option;
  background : string option;
  authors : mal_url list;
  serializations : mal_url list;
  genres : mal_url list;
  explicit_genres : mal_url list;
  themes : mal_url list;
  demographics : mal_url list;
}

type manga_full = {
  manga : manga;
  relations : relation list;
  external_links : external_link list;
}

type character = {
  mal_id : Mal_id.t;
  url : string;
  images : image_pair;
  name : string;
  name_kanji : string option;
  nicknames : string list;
  favorites : int option;
  about : string option;
}

type person = {
  mal_id : Mal_id.t;
  url : string;
  website_url : string option;
  images : image_pair;
  name : string;
  given_name : string option;
  family_name : string option;
  alternate_names : string list;
  birthday : date_range;
  favorites : int option;
  about : string option;
}

type character_role = {
  character : character;
  role : string;
  favorites : int option;
  voice_actors : (person * string) list;
}

type staff_entry = { person : person; positions : string list }

type episode = {
  mal_id : int;
  url : string option;
  title : string;
  title_japanese : string option;
  title_romanji : string option;
  aired : string option;
  score : float option;
  filler : bool;
  recap : bool;
  forum_url : string option;
}

type news_item = {
  mal_id : int;
  url : string;
  title : string;
  date : string option;
  author_username : string option;
  author_url : string option;
  forum_url : string option;
  images : image_pair;
  comments : int option;
  excerpt : string option;
}

type forum_topic = {
  mal_id : int;
  url : string;
  title : string;
  date : string option;
  author_username : string option;
  author_url : string option;
  comments : int option;
}

type promo_video = {
  title : string;
  trailer : trailer;
}

type video_episode = {
  mal_id : int;
  title : string;
  episode : string option;
  url : string option;
  images : image_pair;
}

type videos = {
  promo : promo_video list;
  episodes : video_episode list;
  music_videos : promo_video list;
}

type picture = image_pair

type stats_score = { score : int; votes : int; percentage : float }

type anime_statistics = {
  watching : int option;
  completed : int option;
  on_hold : int option;
  dropped : int option;
  plan_to_watch : int option;
  total : int option;
  scores : stats_score list;
}

type manga_statistics = {
  reading : int option;
  completed : int option;
  on_hold : int option;
  dropped : int option;
  plan_to_read : int option;
  total : int option;
  scores : stats_score list;
}

type recommendation = {
  mal_id : string;
  entry : mal_url * image_pair;
  url : string option;
  votes : int option;
}

type user_update = {
  user : string * string * image_pair;
  score : int option;
  status : string option;
  episodes_seen : int option;
  episodes_total : int option;
  chapters_read : int option;
  chapters_total : int option;
  date : string option;
}

type review = {
  mal_id : int;
  url : string option;
  type_ : string option;
  reactions : (string * int) list;
  date : string option;
  review : string option;
  score : int option;
  tags : string list;
  is_spoiler : bool;
  is_preliminary : bool;
  episodes_watched : int option;
  chapters_read : int option;
  user : string * string * image_pair;
}

type producer = {
  mal_id : Mal_id.t;
  url : string;
  titles : title list;
  images : image_pair;
  established : string option;
  about : string option;
  count : int option;
}

type club = {
  mal_id : Mal_id.t;
  url : string;
  images : image_pair;
  name : string;
  members : int option;
  category : string option;
  created : string option;
  access : string option;
}

type user_profile = {
  mal_id : int option;
  username : string;
  url : string;
  images : image_pair;
  last_online : string option;
  gender : string option;
  birthday : string option;
  location : string option;
  joined : string option;
}

type named_count = { name : string; url : string option; count : int option }

type user_stats = {
  days_watched : float option;
  mean_score : float option;
  watching : int option;
  completed : int option;
  on_hold : int option;
  dropped : int option;
  plan_to_watch : int option;
  total_entries : int option;
  rewatched : int option;
  episodes_watched : int option;
  days_read : float option;
  reading : int option;
  plan_to_read : int option;
  reread : int option;
  chapters_read : int option;
  volumes_read : int option;
}

type watch_episode = {
  entry : mal_url * image_pair;
  episodes : video_episode list;
  region_locked : bool;
}

let images_to_yojson (i : images) =
  `Assoc
    [
      ("image_url", Json.opt_string i.image_url);
      ("small_image_url", Json.opt_string i.small_image_url);
      ("large_image_url", Json.opt_string i.large_image_url);
    ]

let image_pair_to_yojson (p : image_pair) =
  `Assoc [ ("jpg", images_to_yojson p.jpg); ("webp", images_to_yojson p.webp) ]

let empty_images = { image_url = None; small_image_url = None; large_image_url = None }

let empty_image_pair = { jpg = empty_images; webp = empty_images }

let trailer_images_to_yojson (i : trailer_images) =
  `Assoc
    [
      ("image_url", Json.opt_string i.image_url);
      ("small_image_url", Json.opt_string i.small_image_url);
      ("medium_image_url", Json.opt_string i.medium_image_url);
      ("large_image_url", Json.opt_string i.large_image_url);
      ("maximum_image_url", Json.opt_string i.maximum_image_url);
    ]

let empty_trailer_images =
  {
    image_url = None;
    small_image_url = None;
    medium_image_url = None;
    large_image_url = None;
    maximum_image_url = None;
  }

let trailer_to_yojson (t : trailer) =
  `Assoc
    [
      ("youtube_id", Json.opt_string t.youtube_id);
      ("url", Json.opt_string t.url);
      ("embed_url", Json.opt_string t.embed_url);
      ("images", trailer_images_to_yojson t.images);
    ]

let empty_trailer =
  {
    youtube_id = None;
    url = None;
    embed_url = None;
    images = empty_trailer_images;
  }

let title_to_yojson (t : title) =
  `Assoc [ ("type", `String t.type_); ("title", `String t.title) ]

let date_prop_to_yojson (p : date_prop) =
  `Assoc
    [
      ("day", Json.opt_int p.day);
      ("month", Json.opt_int p.month);
      ("year", Json.opt_int p.year);
    ]

let empty_date_prop = { day = None; month = None; year = None }

let date_range_to_yojson (d : date_range) =
  `Assoc
    [
      ("from", Json.opt_string d.from_iso);
      ("to", Json.opt_string d.to_iso);
      ( "prop",
        `Assoc
          [
            ("from", date_prop_to_yojson d.from_prop);
            ("to", date_prop_to_yojson d.to_prop);
          ] );
      ("string", Json.opt_string d.string_);
    ]

let empty_date_range =
  {
    from_iso = None;
    to_iso = None;
    from_prop = empty_date_prop;
    to_prop = empty_date_prop;
    string_ = None;
  }

let broadcast_to_yojson (b : broadcast) =
  `Assoc
    [
      ("day", Json.opt_string b.day);
      ("time", Json.opt_string b.time);
      ("timezone", Json.opt_string b.timezone);
      ("string", Json.opt_string b.string_);
    ]

let empty_broadcast =
  { day = None; time = None; timezone = None; string_ = None }

let mal_url_to_yojson (u : mal_url) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson u.mal_id);
      ("type", `String u.type_);
      ("name", `String u.name);
      ("url", `String u.url);
    ]

let mal_url_images_to_yojson ((u, imgs) : mal_url * image_pair) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson u.mal_id);
      ("url", `String u.url);
      ("images", image_pair_to_yojson imgs);
      ("title", `String u.name);
    ]

let external_link_to_yojson (e : external_link) =
  `Assoc [ ("name", `String e.name); ("url", `String e.url) ]

let relation_to_yojson (r : relation) =
  `Assoc
    [
      ("relation", `String r.relation);
      ("entry", Json.list mal_url_to_yojson r.entry);
    ]

let anime_to_yojson (a : anime) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson a.mal_id);
      ("url", `String a.url);
      ("images", image_pair_to_yojson a.images);
      ("trailer", trailer_to_yojson a.trailer);
      ("approved", `Bool a.approved);
      ("titles", Json.list title_to_yojson a.titles);
      ("title", `String a.title);
      ("title_english", Json.opt_string a.title_english);
      ("title_japanese", Json.opt_string a.title_japanese);
      ("title_synonyms", Json.string_list a.title_synonyms);
      ("type", Json.opt_string a.type_);
      ("source", Json.opt_string a.source);
      ("episodes", Json.opt_int a.episodes);
      ("status", Json.opt_string a.status);
      ("airing", `Bool a.airing);
      ("aired", date_range_to_yojson a.aired);
      ("duration", Json.opt_string a.duration);
      ("rating", Json.opt_string a.rating);
      ("score", Json.opt_float a.score);
      ("scored_by", Json.opt_int a.scored_by);
      ("rank", Json.opt_int a.rank);
      ("popularity", Json.opt_int a.popularity);
      ("members", Json.opt_int a.members);
      ("favorites", Json.opt_int a.favorites);
      ("synopsis", Json.opt_string a.synopsis);
      ("background", Json.opt_string a.background);
      ("season", Json.opt_string a.season);
      ("year", Json.opt_int a.year);
      ("broadcast", broadcast_to_yojson a.broadcast);
      ("producers", Json.list mal_url_to_yojson a.producers);
      ("licensors", Json.list mal_url_to_yojson a.licensors);
      ("studios", Json.list mal_url_to_yojson a.studios);
      ("genres", Json.list mal_url_to_yojson a.genres);
      ("explicit_genres", Json.list mal_url_to_yojson a.explicit_genres);
      ("themes", Json.list mal_url_to_yojson a.themes);
      ("demographics", Json.list mal_url_to_yojson a.demographics);
    ]

let anime_full_to_yojson (f : anime_full) =
  match anime_to_yojson f.anime with
  | `Assoc fields ->
      let openings, endings = f.theme in
      `Assoc
        (fields
        @ [
            ("relations", Json.list relation_to_yojson f.relations);
            ( "theme",
              `Assoc
                [
                  ("openings", Json.string_list openings);
                  ("endings", Json.string_list endings);
                ] );
            ("external", Json.list external_link_to_yojson f.external_links);
            ("streaming", Json.list external_link_to_yojson f.streaming);
          ])
  | json -> json

let manga_to_yojson (m : manga) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson m.mal_id);
      ("url", `String m.url);
      ("images", image_pair_to_yojson m.images);
      ("approved", `Bool m.approved);
      ("titles", Json.list title_to_yojson m.titles);
      ("title", `String m.title);
      ("title_english", Json.opt_string m.title_english);
      ("title_japanese", Json.opt_string m.title_japanese);
      ("title_synonyms", Json.string_list m.title_synonyms);
      ("type", Json.opt_string m.type_);
      ("chapters", Json.opt_int m.chapters);
      ("volumes", Json.opt_int m.volumes);
      ("status", Json.opt_string m.status);
      ("publishing", `Bool m.publishing);
      ("published", date_range_to_yojson m.published);
      ("score", Json.opt_float m.score);
      ("scored_by", Json.opt_int m.scored_by);
      ("rank", Json.opt_int m.rank);
      ("popularity", Json.opt_int m.popularity);
      ("members", Json.opt_int m.members);
      ("favorites", Json.opt_int m.favorites);
      ("synopsis", Json.opt_string m.synopsis);
      ("background", Json.opt_string m.background);
      ("authors", Json.list mal_url_to_yojson m.authors);
      ("serializations", Json.list mal_url_to_yojson m.serializations);
      ("genres", Json.list mal_url_to_yojson m.genres);
      ("explicit_genres", Json.list mal_url_to_yojson m.explicit_genres);
      ("themes", Json.list mal_url_to_yojson m.themes);
      ("demographics", Json.list mal_url_to_yojson m.demographics);
    ]

let manga_full_to_yojson (f : manga_full) =
  match manga_to_yojson f.manga with
  | `Assoc fields ->
      `Assoc
        (fields
        @ [
            ("relations", Json.list relation_to_yojson f.relations);
            ("external", Json.list external_link_to_yojson f.external_links);
          ])
  | json -> json

let character_to_yojson (c : character) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson c.mal_id);
      ("url", `String c.url);
      ("images", image_pair_to_yojson c.images);
      ("name", `String c.name);
      ("name_kanji", Json.opt_string c.name_kanji);
      ("nicknames", Json.string_list c.nicknames);
      ("favorites", Json.opt_int c.favorites);
      ("about", Json.opt_string c.about);
    ]

let person_to_yojson (p : person) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson p.mal_id);
      ("url", `String p.url);
      ("website_url", Json.opt_string p.website_url);
      ("images", image_pair_to_yojson p.images);
      ("name", `String p.name);
      ("given_name", Json.opt_string p.given_name);
      ("family_name", Json.opt_string p.family_name);
      ("alternate_names", Json.string_list p.alternate_names);
      ("birthday", date_range_to_yojson p.birthday);
      ("favorites", Json.opt_int p.favorites);
      ("about", Json.opt_string p.about);
    ]

let character_mini_to_yojson (c : character) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson c.mal_id);
      ("url", `String c.url);
      ("images", image_pair_to_yojson c.images);
      ("name", `String c.name);
    ]

let person_mini_to_yojson (p : person) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson p.mal_id);
      ("url", `String p.url);
      ("images", image_pair_to_yojson p.images);
      ("name", `String p.name);
    ]

let character_role_to_yojson (c : character_role) =
  `Assoc
    [
      ("character", character_mini_to_yojson c.character);
      ("role", `String c.role);
      ("favorites", Json.opt_int c.favorites);
      ( "voice_actors",
        Json.list
          (fun (person, language) ->
            `Assoc
              [
                ("person", person_mini_to_yojson person);
                ("language", `String language);
              ])
          c.voice_actors );
    ]

let staff_entry_to_yojson (s : staff_entry) =
  `Assoc
    [
      ("person", person_mini_to_yojson s.person);
      ("positions", Json.string_list s.positions);
    ]

let episode_to_yojson (e : episode) =
  `Assoc
    [
      ("mal_id", `Int e.mal_id);
      ("url", Json.opt_string e.url);
      ("title", `String e.title);
      ("title_japanese", Json.opt_string e.title_japanese);
      ("title_romanji", Json.opt_string e.title_romanji);
      ("aired", Json.opt_string e.aired);
      ("score", Json.opt_float e.score);
      ("filler", `Bool e.filler);
      ("recap", `Bool e.recap);
      ("forum_url", Json.opt_string e.forum_url);
    ]

let news_item_to_yojson (n : news_item) =
  `Assoc
    [
      ("mal_id", `Int n.mal_id);
      ("url", `String n.url);
      ("title", `String n.title);
      ("date", Json.opt_string n.date);
      ("author_username", Json.opt_string n.author_username);
      ("author_url", Json.opt_string n.author_url);
      ("forum_url", Json.opt_string n.forum_url);
      ("images", image_pair_to_yojson n.images);
      ("comments", Json.opt_int n.comments);
      ("excerpt", Json.opt_string n.excerpt);
    ]

let forum_topic_to_yojson (t : forum_topic) =
  `Assoc
    [
      ("mal_id", `Int t.mal_id);
      ("url", `String t.url);
      ("title", `String t.title);
      ("date", Json.opt_string t.date);
      ("author_username", Json.opt_string t.author_username);
      ("author_url", Json.opt_string t.author_url);
      ("comments", Json.opt_int t.comments);
    ]

let promo_video_to_yojson (p : promo_video) =
  `Assoc [ ("title", `String p.title); ("trailer", trailer_to_yojson p.trailer) ]

let video_episode_to_yojson (e : video_episode) =
  `Assoc
    [
      ("mal_id", `Int e.mal_id);
      ("title", `String e.title);
      ("episode", Json.opt_string e.episode);
      ("url", Json.opt_string e.url);
      ("images", image_pair_to_yojson e.images);
    ]

let videos_to_yojson (v : videos) =
  `Assoc
    [
      ("promo", Json.list promo_video_to_yojson v.promo);
      ("episodes", Json.list video_episode_to_yojson v.episodes);
      ("music_videos", Json.list promo_video_to_yojson v.music_videos);
    ]

let picture_to_yojson (p : picture) =
  `Assoc [ ("jpg", images_to_yojson p.jpg); ("webp", images_to_yojson p.webp) ]

let stats_score_to_yojson (s : stats_score) =
  `Assoc
    [
      ("score", `Int s.score);
      ("votes", `Int s.votes);
      ("percentage", `Float s.percentage);
    ]

let anime_statistics_to_yojson (s : anime_statistics) =
  `Assoc
    [
      ("watching", Json.opt_int s.watching);
      ("completed", Json.opt_int s.completed);
      ("on_hold", Json.opt_int s.on_hold);
      ("dropped", Json.opt_int s.dropped);
      ("plan_to_watch", Json.opt_int s.plan_to_watch);
      ("total", Json.opt_int s.total);
      ("scores", Json.list stats_score_to_yojson s.scores);
    ]

let manga_statistics_to_yojson (s : manga_statistics) =
  `Assoc
    [
      ("reading", Json.opt_int s.reading);
      ("completed", Json.opt_int s.completed);
      ("on_hold", Json.opt_int s.on_hold);
      ("dropped", Json.opt_int s.dropped);
      ("plan_to_read", Json.opt_int s.plan_to_read);
      ("total", Json.opt_int s.total);
      ("scores", Json.list stats_score_to_yojson s.scores);
    ]

let recommendation_to_yojson (r : recommendation) =
  let entry, images = r.entry in
  `Assoc
    [
      ("mal_id", `String r.mal_id);
      ("entry", mal_url_images_to_yojson (entry, images));
      ("url", Json.opt_string r.url);
      ("votes", Json.opt_int r.votes);
    ]

let user_update_to_yojson (u : user_update) =
  let username, url, images = u.user in
  `Assoc
    [
      ( "user",
        `Assoc
          [
            ("username", `String username);
            ("url", `String url);
            ("images", image_pair_to_yojson images);
          ] );
      ("score", Json.opt_int u.score);
      ("status", Json.opt_string u.status);
      ("episodes_seen", Json.opt_int u.episodes_seen);
      ("episodes_total", Json.opt_int u.episodes_total);
      ("chapters_read", Json.opt_int u.chapters_read);
      ("chapters_total", Json.opt_int u.chapters_total);
      ("date", Json.opt_string u.date);
    ]

let review_to_yojson (r : review) =
  let username, url, images = r.user in
  let reactions =
    `Assoc (List.map (fun (k, v) -> (k, `Int v)) r.reactions)
  in
  `Assoc
    [
      ("mal_id", `Int r.mal_id);
      ("url", Json.opt_string r.url);
      ("type", Json.opt_string r.type_);
      ("reactions", reactions);
      ("date", Json.opt_string r.date);
      ("review", Json.opt_string r.review);
      ("score", Json.opt_int r.score);
      ("tags", Json.string_list r.tags);
      ("is_spoiler", `Bool r.is_spoiler);
      ("is_preliminary", `Bool r.is_preliminary);
      ("episodes_watched", Json.opt_int r.episodes_watched);
      ("chapters_read", Json.opt_int r.chapters_read);
      ( "user",
        `Assoc
          [
            ("username", `String username);
            ("url", `String url);
            ("images", image_pair_to_yojson images);
          ] );
    ]

let producer_to_yojson (p : producer) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson p.mal_id);
      ("url", `String p.url);
      ("titles", Json.list title_to_yojson p.titles);
      ("images", image_pair_to_yojson p.images);
      ("established", Json.opt_string p.established);
      ("about", Json.opt_string p.about);
      ("count", Json.opt_int p.count);
    ]

let club_to_yojson (c : club) =
  `Assoc
    [
      ("mal_id", Mal_id.to_yojson c.mal_id);
      ("url", `String c.url);
      ("images", image_pair_to_yojson c.images);
      ("name", `String c.name);
      ("members", Json.opt_int c.members);
      ("category", Json.opt_string c.category);
      ("created", Json.opt_string c.created);
      ("access", Json.opt_string c.access);
    ]

let user_profile_to_yojson (u : user_profile) =
  `Assoc
    [
      ("mal_id", Json.opt_int u.mal_id);
      ("username", `String u.username);
      ("url", `String u.url);
      ("images", image_pair_to_yojson u.images);
      ("last_online", Json.opt_string u.last_online);
      ("gender", Json.opt_string u.gender);
      ("birthday", Json.opt_string u.birthday);
      ("location", Json.opt_string u.location);
      ("joined", Json.opt_string u.joined);
    ]

let named_count_to_yojson (n : named_count) =
  `Assoc
    [
      ("name", `String n.name);
      ("url", Json.opt_string n.url);
      ("count", Json.opt_int n.count);
    ]

let user_stats_to_yojson (s : user_stats) =
  `Assoc
    [
      ( "anime",
        `Assoc
          [
            ("days_watched", Json.opt_float s.days_watched);
            ("mean_score", Json.opt_float s.mean_score);
            ("watching", Json.opt_int s.watching);
            ("completed", Json.opt_int s.completed);
            ("on_hold", Json.opt_int s.on_hold);
            ("dropped", Json.opt_int s.dropped);
            ("plan_to_watch", Json.opt_int s.plan_to_watch);
            ("total_entries", Json.opt_int s.total_entries);
            ("rewatched", Json.opt_int s.rewatched);
            ("episodes_watched", Json.opt_int s.episodes_watched);
          ] );
      ( "manga",
        `Assoc
          [
            ("days_read", Json.opt_float s.days_read);
            ("mean_score", Json.opt_float s.mean_score);
            ("reading", Json.opt_int s.reading);
            ("completed", Json.opt_int s.completed);
            ("on_hold", Json.opt_int s.on_hold);
            ("dropped", Json.opt_int s.dropped);
            ("plan_to_read", Json.opt_int s.plan_to_read);
            ("total_entries", Json.opt_int s.total_entries);
            ("reread", Json.opt_int s.reread);
            ("chapters_read", Json.opt_int s.chapters_read);
            ("volumes_read", Json.opt_int s.volumes_read);
          ] );
    ]

let watch_episode_to_yojson (w : watch_episode) =
  let entry, images = w.entry in
  `Assoc
    [
      ("entry", mal_url_images_to_yojson (entry, images));
      ("episodes", Json.list video_episode_to_yojson w.episodes);
      ("region_locked", `Bool w.region_locked);
    ]
