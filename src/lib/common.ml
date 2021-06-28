open Types

(* Convenience functions for throwing exceptions *)
let key_error err = raise (Key_error err)
let type_error err = raise (Type_error err)

let type_string v =
  match v with
  | TomlString _ -> "string"
  | TomlInteger _ -> "integer"
  | TomlFloat _ -> "float"
  | TomlBoolean _ -> "boolean"
  | TomlLocalTime _ -> "local time"
  | TomlLocalDate _ -> "local date"
  | TomlLocalDateTime _ -> "local date-time"
  | TomlOffsetDateTime _ -> "offset date-time"
  | TomlArray _ -> "array"
  | TomlTable _ | TomlInlineTable _ | TomlTableArray _ -> "table"
