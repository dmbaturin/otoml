module type TomlInteger = sig
  type t

  val to_string : t -> string
  val of_string : string -> t

  val to_boolean : t -> bool
  val of_boolean : bool -> t
end

module type TomlFloat = sig
  type t

  val to_string : t -> string
  val of_string : string -> t

  val to_boolean : t -> bool
  val of_boolean : bool -> t
end

module type TomlDate = sig
  type t

  val local_time_of_string : string -> t
  val local_date_of_string : string -> t
  val local_datetime_of_string : string -> t
  val offset_datetime_of_string : string -> t

  val local_time_to_string : t -> string
  val local_date_to_string : t -> string
  val local_datetime_to_string : t -> string
  val offset_datetime_to_string : t -> string 
end

module type TomlImplementation = sig
  type toml_integer
  type toml_float
  type toml_date

  type t =
  | TomlString of string
  | TomlInteger of toml_integer
  | TomlFloat of toml_float
  | TomlBoolean of bool
  | TomlOffsetDateTime of toml_date
  | TomlLocalDateTime of toml_date
  | TomlLocalDate of toml_date
  | TomlLocalTime of toml_date
  | TomlArray of t list
  | TomlTable of (string * t) list
  | TomlInlineTable of (string * t) list
  | TomlTableArray of t list

  module Printer : sig
    val to_string :
      ?indent_width:int -> ?indent_character:char -> ?indent_subtables:bool ->
      ?newline_before_table:bool -> ?collapse_tables:bool ->
      t -> string

    val to_channel :
      ?indent_width:int -> ?indent_character:char -> ?indent_subtables:bool ->
      ?newline_before_table:bool -> ?collapse_tables:bool ->
      out_channel -> t -> unit
  end

  module Parser : sig
    val from_file : string -> t
    val from_channel : in_channel -> t
    val from_string : string -> t

    val from_file_result : string -> (t, string) result
    val from_channel_result : in_channel -> (t, string) result
    val from_string_result : string -> (t, string) result

    val format_parse_error : (int * int) option -> string -> string
  end

  (** Constructors *)

  val string : string -> t
  val integer : toml_integer -> t
  val float : toml_float -> t
  val boolean : bool -> t
  val offset_datetime : toml_date -> t
  val local_datetime : toml_date -> t
  val local_date : toml_date -> t
  val local_time : toml_date -> t
  val array : t list -> t
  val table : (string * t) list -> t
  val inline_table : (string * t) list -> t

  (** Accessors *)

  val get_value : t -> t
  val get_table : t -> (string * t) list

  (** In non-strict mode, forces a value [x] to a single-item array [[x]] *) 
  val get_array : ?strict:bool -> t -> t list

  val get_string : ?strict:bool -> t -> string
  val get_integer : ?strict:bool -> t -> toml_integer
  val get_float : ?strict:bool -> t -> toml_float
  val get_boolean : ?strict:bool -> t -> bool

  val get_offset_datetime : t -> toml_date
  val get_local_datetime : t -> toml_date
  val get_datetime : t -> toml_date
  val get_local_date : t -> toml_date
  val get_date : t -> toml_date
  val get_local_time : t -> toml_date


  (** High-level interface *)

  val list_table_keys : t -> string list

  val find : t -> (t -> 'a) -> string list -> 'a

  val find_opt : t -> (t -> 'a) -> string list -> 'a option

  val find_or : default:'a -> t -> (t -> 'a) -> string list -> 'a

  val find_result : t -> (t -> 'a) -> string list -> ('a, string) result

  val update : ?use_inline_tables:bool -> t -> string list -> t option -> t
end

