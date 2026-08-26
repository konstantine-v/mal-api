type t =
  | Bad_request of string
  | Not_found
  | Rate_limited
  | Mal_unavailable
  | Parse_error of string
  | Internal of string

let status = function
  | Bad_request _ -> 400
  | Not_found -> 404
  | Rate_limited -> 429
  | Mal_unavailable -> 503
  | Parse_error _ | Internal _ -> 500

let type_name = function
  | Bad_request _ -> "BadRequestException"
  | Not_found -> "NotFoundException"
  | Rate_limited -> "RateLimitException"
  | Mal_unavailable -> "ServiceUnavailableException"
  | Parse_error _ -> "ParserException"
  | Internal _ -> "InternalException"

let message = function
  | Bad_request msg -> msg
  | Not_found -> "Resource does not exist"
  | Rate_limited -> "Rate limited by MyAnimeList"
  | Mal_unavailable -> "MyAnimeList is unavailable"
  | Parse_error msg -> msg
  | Internal msg -> msg

let to_yojson t =
  let code = status t in
  `Assoc
    [
      ("status", `Int code);
      ("type", `String (type_name t));
      ("message", `String (message t));
      ("error", `String (message t));
    ]

let dream_status t : Dream.status =
  match t with
  | Bad_request _ -> `Bad_Request
  | Not_found -> `Not_Found
  | Rate_limited -> `Too_Many_Requests
  | Mal_unavailable -> `Service_Unavailable
  | Parse_error _ | Internal _ -> `Internal_Server_Error

let respond t =
  Dream.json ~status:(dream_status t) (Yojson.Safe.to_string (to_yojson t))
