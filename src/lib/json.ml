open Types

let rec of_json j =
  match j with
  | `Float n -> TomlFloat n
  | `Bool b -> TomlBoolean b
  | `String s -> TomlString s
  | `A js -> TomlArray (List.map of_json js)
  | `O os -> TomlTable (List.map (fun (k, v) -> (k, of_json v)) os)
  | `Null -> TomlTable []

let rec to_json t =
  match t with
  | TomlString s -> `String s
  | TomlInteger i -> `Float (float_of_int i)
  | TomlFloat f -> `Float f
  | TomlBoolean b -> `Bool b
  | TomlLocalTime s -> `String s
  | TomlLocalDate s -> `String s
  | TomlLocalDateTime s -> `String s
  | TomlOffsetDateTime s -> `String s
  | TomlArray xs | TomlTableArray xs -> `A (List.map to_json xs)
  | TomlTable os | TomlInlineTable os -> `O (List.map (fun (k, v) -> (k, to_json v)) os)
