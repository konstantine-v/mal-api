type rpc_id =
  | Id_int of int
  | Id_string of string
  | Id_null

type incoming =
  | Request of { id : rpc_id; method_ : string; params : Yojson.Safe.t }
  | Notification of { method_ : string; params : Yojson.Safe.t }

let supported_versions =
  [ "2024-11-05"; "2025-03-26"; "2025-06-18"; "2025-11-25" ]

let default_version = "2025-03-26"

let id_to_yojson = function
  | Id_int n -> `Int n
  | Id_string s -> `String s
  | Id_null -> `Null

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let parse_id = function
  | `Int n -> Id_int n
  | `String s -> Id_string s
  | `Null -> Id_null
  | `Float f when float_of_int (int_of_float f) = f -> Id_int (int_of_float f)
  | _ -> Id_null

let parse_message json =
  match (member "jsonrpc" json, member "method" json) with
  | Some (`String "2.0"), Some (`String method_) ->
      let params = Option.value (member "params" json) ~default:(`Assoc []) in
      (match member "id" json with
      | None -> Ok (Notification { method_; params })
      | Some id -> Ok (Request { id = parse_id id; method_; params }))
  | Some (`String "2.0"), _ -> Error "missing method"
  | _ -> Error "invalid jsonrpc"

let jsonrpc_error ~id ~code ~message =
  let id_json = match id with None -> `Null | Some id -> id_to_yojson id in
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id_json);
      ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]);
    ]

let jsonrpc_result id result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id_to_yojson id); ("result", result) ]

let json_response ?(status = `OK) body =
  Dream.json ~status (Yojson.Safe.to_string body)

let protocol_version params =
  match member "protocolVersion" params with
  | Some (`String v) when List.mem v supported_versions -> v
  | _ -> default_version

let initialize_result app params =
  `Assoc
    [
      ("protocolVersion", `String (protocol_version params));
      ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
      ( "serverInfo",
        `Assoc
          [
            ("name", `String "mal-api");
            ("version", `String app.App.cfg.app_version);
          ] );
      ("instructions", `String "Each tool is a GET /api MAL API route.");
    ]

let tool_result ~is_error text =
  `Assoc
    [
      ( "content",
        `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ] );
      ("isError", `Bool is_error);
    ]

let tools_list () =
  `Assoc
    [
      ( "tools",
        `List
          (List.map
             (fun t ->
               `Assoc
                 [
                   ("name", `String t.Mcp_tools.name);
                   ("description", `String t.description);
                   ("inputSchema", t.input_schema);
                 ])
             Mcp_tools.all) );
    ]

let call_tool app params =
  let name =
    match member "name" params with
    | Some (`String n) -> n
    | _ -> ""
  in
  let args = Option.value (member "arguments" params) ~default:(`Assoc []) in
  match Mcp_tools.find name with
  | None -> Lwt.return (Error (`Invalid_params ("unknown tool: " ^ name)))
  | Some t -> (
      match%lwt t.run app args with
      | Ok json -> Lwt.return (Ok (tool_result ~is_error:false (Yojson.Safe.pretty_to_string json)))
      | Error e -> Lwt.return (Ok (tool_result ~is_error:true (Error.message e))))

let handle_request app ~id method_ params =
  match method_ with
  | "initialize" -> Lwt.return (jsonrpc_result id (initialize_result app params))
  | "ping" -> Lwt.return (jsonrpc_result id (`Assoc []))
  | "tools/list" -> Lwt.return (jsonrpc_result id (tools_list ()))
  | "tools/call" -> (
      match%lwt call_tool app params with
      | Ok result -> Lwt.return (jsonrpc_result id result)
      | Error (`Invalid_params msg) ->
          Lwt.return (jsonrpc_error ~id:(Some id) ~code:(-32602) ~message:msg))
  | _ -> Lwt.return (jsonrpc_error ~id:(Some id) ~code:(-32601) ~message:("method not found: " ^ method_))

let post app req =
  let%lwt body = Dream.body req in
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ ->
      json_response ~status:`Bad_Request
        (jsonrpc_error ~id:None ~code:(-32700) ~message:"parse error")
  | `List _ ->
      json_response ~status:`Bad_Request
        (jsonrpc_error ~id:None ~code:(-32600) ~message:"JSON-RPC batches are not supported")
  | json -> (
      match parse_message json with
      | Error msg ->
          json_response ~status:`Bad_Request
            (jsonrpc_error ~id:None ~code:(-32600) ~message:msg)
      | Ok (Notification _) -> Dream.empty `Accepted
      | Ok (Request { id; method_; params }) ->
          let%lwt body = handle_request app ~id method_ params in
          json_response body)

let get _req = Dream.empty `Method_Not_Allowed

let delete _req = Dream.empty `OK
