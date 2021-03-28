type t =
  | TomlString of string
  | TomlInteger of int
  | TomlFloat of float
  | TomlBoolean of bool
  | TomlLocalTime of string
  | TomlLocalDate of string
  | TomlLocalDateTime of string
  | TomlOffsetDateTime of string
  | TomlArray of t list
  | TomlTable of (string * t) list
  | TomlInlineTable of (string * t) list 
  | TomlTableArray of (string * t) list
