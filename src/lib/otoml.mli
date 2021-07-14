(** Default TOML implementation. *)

(** Raised when a TOML table does not have specified key. *)
exception Key_error of string

(** Raised when an operation is performed with an incompatible TOML type. *)
exception Type_error of string

(** Raised when a TOML document cannot be parsed due to syntax or semantic errors. *)
exception Parse_error of ((int * int) option * string)


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

module Printer : sig
  val to_string :
    ?indent_width:int -> ?indent_character:char -> ?indent_subtables:bool -> ?newline_before_table:bool ->
    t -> string

    val to_channel :
    ?indent_width:int -> ?indent_character:char -> ?indent_subtables:bool -> ?newline_before_table:bool ->
    out_channel -> t -> unit
end

module Parser : sig
  val from_file : string -> t
  val from_channel : in_channel -> t
  val from_string : string -> t

  val from_file_result : string -> (t, string) result
  val from_channel_result : in_channel -> (t, string) result
  val from_string_result : string -> (t, string) result
end

(** Constructors *)

val string : string -> t
val integer : int -> t
val float : float -> t
val boolean : bool -> t
val offset_datetime : string -> t
val local_datetime : string -> t
val local_date : string -> t
val local_time : string -> t
val array : t list -> t
val table : (string * t) list -> t
val inline_table : (string * t) list -> t

(** Accessors *)

val get_value : t -> t
val get_table : t -> (string * t) list

(** In non-strict mode, forces a value [x] to a single-item array [[x]] *) 
val get_array : ?strict:bool -> t -> t list

val get_string : ?strict:bool -> t -> string
val get_integer : ?strict:bool -> t -> int
val get_float : ?strict:bool -> t -> float
val get_boolean : ?strict:bool -> t -> bool

(** High-level interface *)

val find : (t -> 'a) -> t -> string list -> 'a

val find_opt : (t -> 'a) -> t -> string list -> 'a option

val find_or : default:'a -> (t -> 'a) -> t -> string list -> 'a

val find_result : (t -> 'a) -> t -> string list -> ('a, string) result

val update : ?use_inline_tables:bool -> t -> string list -> t option -> t


