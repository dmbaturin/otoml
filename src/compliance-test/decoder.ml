open Otoml

let type_string v =
  match v with
  | TomlString _ -> "string"
  | TomlInteger _ -> "integer"
  | TomlFloat _ -> "float"
  | TomlBoolean _ -> "bool"
  | TomlLocalTime _ -> "local-time"
  | TomlLocalDate _ -> "local-date"
  | TomlLocalDateTime _ -> "local date-time"
  | TomlOffsetDateTime _ -> "offset date-time"
  | TomlArray _ -> "array"
  | TomlTable _ | TomlInlineTable _ | TomlTableArray _ -> "table"


let rec to_json t =
  match t with
  | TomlTable kvs ->
    `Assoc (List.map (fun (k, v) -> (k, json_of_value v)) kvs)
  | TomlInlineTable kvs ->
    `Assoc (List.map (fun (k, v) -> (k, json_of_value v)) kvs)
  | TomlArray vs ->
    `List (List.map json_of_value vs)
  | TomlTableArray vs ->
    `List (List.map json_of_value vs)
  | _ -> failwith "bad type"
and json_of_value v =
  let typ = ("type", `String (type_string v)) in
  match v with
  | TomlInteger i -> `Assoc [typ; "value", `String (string_of_int i)]
  | TomlFloat f -> `Assoc [typ; "value", `String (string_of_float f)]
  | TomlString s -> `Assoc [typ; "value", `String ( s)]
  | TomlBoolean b -> `Assoc [typ; "value", `String (string_of_bool b)]
  | TomlTable _ as t -> to_json t
  | TomlInlineTable _ as t -> to_json t
  | TomlArray _ as a -> to_json a
  | TomlTableArray _ as a -> to_json a
  | _ -> failwith "unimplemented"

let () =
  let res = Parser.from_channel stdin in
  match res with
  | Ok toml ->
    let json = to_json toml in
    Yojson.to_channel stdout json
  | Error err ->
    Printf.eprintf "%s" err;
    exit 1
