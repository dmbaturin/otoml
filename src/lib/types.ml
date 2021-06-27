(* Errors *)

exception Key_error of string
exception Type_error of string

(* Types *)

type t =
  | TomlString of string
  | TomlInteger of int
  | TomlFloat of float
  | TomlBoolean of bool
  | TomlOffsetDateTime of string
  | TomlLocalDateTime of string
  | TomlLocalDate of string
  | TomlLocalTime of string
  | TomlArray of t list
  | TomlTable of (string * t) list
  | TomlInlineTable of (string * t) list
  | TomlTableArray of t list
