type log_level = Logs.level

type t = {
  port : int;
  bind : string;
  app_version : string;
  github_url : string;
  cache_path : string;
  cache_default_expire : int;
  cache_meta_expire : int;
  cache_user_expire : int;
  cache_userlist_expire : int;
  cache_404_expire : int;
  cache_search_expire : int;
  cache_producers_expire : int;
  cache_magazines_expire : int;
  max_results_per_page : int;
  source_timeout : float;
  source_delay : float;
  source_bad_health_threshold : int;
  source_bad_health_recheck : float;
  source_bad_health_range : float;
  source_good_health_score : float;
  mal_user_agent : string;
  disable_user_lists : bool;
  log_level : Logs.level option;
}

let getenv name = Sys.getenv_opt name

let getenv_default name default =
  match getenv name with
  | None | Some "" -> default
  | Some v -> v

let getenv_int name default =
  match getenv name with
  | None | Some "" -> default
  | Some v -> (
      match int_of_string_opt v with
      | Some n -> n
      | None -> default)

let getenv_float name default =
  match getenv name with
  | None | Some "" -> default
  | Some v -> (
      match float_of_string_opt v with
      | Some n -> n
      | None -> default)

let getenv_bool name default =
  match getenv name with
  | None | Some "" -> default
  | Some v -> (
      match String.lowercase_ascii (String.trim v) with
      | "1" | "true" | "yes" | "on" -> true
      | "0" | "false" | "no" | "off" -> false
      | _ -> default)

let parse_log_level = function
  | "debug" -> Some Logs.Debug
  | "info" -> Some Logs.Info
  | "warning" | "warn" -> Some Logs.Warning
  | "error" -> Some Logs.Error
  | "app" -> Some Logs.App
  | _ -> Some Logs.Info

let load () =
  let delay_ms = getenv_float "SOURCE_DELAY_MS" 500. in
  {
    port = getenv_int "PORT" 8080;
    bind = getenv_default "BIND" "0.0.0.0";
    app_version = getenv_default "APP_VERSION" "1.1.0";
    github_url = getenv_default "GITHUB_URL" "https://github.com/konstantine-v/mal-api";
    cache_path = getenv_default "CACHE_PATH" "data/mal.db";
    cache_default_expire = getenv_int "CACHE_DEFAULT_EXPIRE" 86400;
    cache_meta_expire = getenv_int "CACHE_META_EXPIRE" 300;
    cache_user_expire = getenv_int "CACHE_USER_EXPIRE" 300;
    cache_userlist_expire = getenv_int "CACHE_USERLIST_EXPIRE" 3600;
    cache_404_expire = getenv_int "CACHE_404_EXPIRE" 604800;
    cache_search_expire = getenv_int "CACHE_SEARCH_EXPIRE" 432000;
    cache_producers_expire = getenv_int "CACHE_PRODUCERS_EXPIRE" 432000;
    cache_magazines_expire = getenv_int "CACHE_MAGAZINES_EXPIRE" 432000;
    max_results_per_page = getenv_int "MAX_RESULTS_PER_PAGE" 25;
    source_timeout = getenv_float "SOURCE_TIMEOUT" 10.;
    source_delay = delay_ms /. 1000.;
    source_bad_health_threshold = getenv_int "SOURCE_BAD_HEALTH_THRESHOLD" 10;
    source_bad_health_recheck = getenv_float "SOURCE_BAD_HEALTH_RECHECK" 10.;
    source_bad_health_range = getenv_float "SOURCE_BAD_HEALTH_RANGE" 30.;
    source_good_health_score = getenv_float "SOURCE_GOOD_HEALTH_SCORE" 0.9;
    mal_user_agent =
      getenv_default "MAL_USER_AGENT"
        "mal-api/1.1.0 (https://github.com/konstantine-v/mal-api)";
    disable_user_lists = getenv_bool "DISABLE_USER_LISTS" false;
    log_level = parse_log_level (String.lowercase_ascii (getenv_default "LOG_LEVEL" "info"));
  }
