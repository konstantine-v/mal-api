let base = "https://myanimelist.net"

let anime id = Printf.sprintf "%s/anime/%d" base (Mal_id.to_int id)

let anime_characters id = anime id ^ "/characters"

let anime_episodes id page =
  let offset = (page - 1) * 100 in
  Printf.sprintf "%s/episode?offset=%d" (anime id) offset

let anime_episode id ep =
  Printf.sprintf "%s/episode/%d" (anime id) ep

let anime_news id page =
  Printf.sprintf "%s/news?p=%d" (anime id) page

let anime_forum id = anime id ^ "/forum"

let anime_videos id = anime id ^ "/video"

let anime_pictures id = anime id ^ "/pics"

let anime_stats id = anime id ^ "/stats"

let anime_recs id = anime id ^ "/userrecs"

let anime_reviews id page =
  Printf.sprintf "%s/reviews?sort=newest&p=%d" (anime id) page

let manga id = Printf.sprintf "%s/manga/%d" base (Mal_id.to_int id)

let manga_characters id = manga id ^ "/characters"

let manga_news id page = Printf.sprintf "%s/news?p=%d" (manga id) page

let manga_forum id = manga id ^ "/forum"

let manga_pictures id = manga id ^ "/pics"

let manga_stats id = manga id ^ "/stats"

let manga_recs id = manga id ^ "/userrecs"

let manga_reviews id page =
  Printf.sprintf "%s/reviews?sort=newest&p=%d" (manga id) page

let character id = Printf.sprintf "%s/character/%d" base (Mal_id.to_int id)

let character_pictures id = character id ^ "/pics"

let people id = Printf.sprintf "%s/people/%d" base (Mal_id.to_int id)

let people_pictures id = people id ^ "/pics"

let season_now = base ^ "/anime/season"

let season_upcoming = base ^ "/anime/season/later"

let season year season =
  Printf.sprintf "%s/anime/season/%d/%s" base year season

let season_archive = base ^ "/anime/season/archive"

let schedule = base ^ "/anime/season/schedule"

let producers = base ^ "/anime/producer"

let producer id = Printf.sprintf "%s/anime/producer/%d" base (Mal_id.to_int id)

let magazines = base ^ "/manga/magazine"

let genres_anime = base ^ "/anime.php"

let genres_manga = base ^ "/manga.php"

let top_anime = base ^ "/topanime.php"

let top_manga = base ^ "/topmanga.php"

let top_characters = base ^ "/character.php?op=list"

let top_people = base ^ "/people.php"

let top_reviews = base ^ "/reviews.php"

let clubs = base ^ "/clubs.php"

let club id = Printf.sprintf "%s/clubs.php?cid=%d" base (Mal_id.to_int id)

let user username = Printf.sprintf "%s/profile/%s" base username

let user_stats username = user username ^ "/statistics"

let user_favorites username = user username ^ "/favorites"

let user_reviews username = user username ^ "/reviews"

let user_clubs username = user username ^ "/clubs"

let user_friends username = user username ^ "/friends"

let user_recs username = user username ^ "/recommendations"

let user_history username = user username ^ "/history"

let user_animelist username = Printf.sprintf "%s/animelist/%s" base username

let user_mangalist username = Printf.sprintf "%s/mangalist/%s" base username

let reviews_anime = base ^ "/reviews.php?t=anime"

let reviews_manga = base ^ "/reviews.php?t=manga"

let recs_anime = base ^ "/recommendations.php?s=recentrecs&t=anime"

let recs_manga = base ^ "/recommendations.php?s=recentrecs&t=manga"

let watch_episodes = base ^ "/watch/episode"

let watch_promos = base ^ "/watch/promotion"

let search_anime q page =
  Printf.sprintf "%s/anime.php?q=%s&cat=anime&show=%d" base
    (Uri.pct_encode q)
    ((page - 1) * 50)

let search_manga q page =
  Printf.sprintf "%s/manga.php?q=%s&cat=manga&show=%d" base
    (Uri.pct_encode q)
    ((page - 1) * 50)

let search_character q page =
  Printf.sprintf "%s/character.php?q=%s&show=%d" base (Uri.pct_encode q)
    ((page - 1) * 50)

let search_people q page =
  Printf.sprintf "%s/people.php?q=%s&show=%d" base (Uri.pct_encode q)
    ((page - 1) * 50)

let search_users q =
  Printf.sprintf "%s/users.php?q=%s" base (Uri.pct_encode q)

let search_clubs q =
  Printf.sprintf "%s/clubs.php?q=%s&action=find" base (Uri.pct_encode q)

let random_anime = base ^ "/anime.php?letter=."
let random_manga = base ^ "/manga.php?letter=."
