module type TomlNumber = sig
  type int
  type float

  val int_of_string : string -> int
  val int_to_string : int -> string

  val float_of_string : string -> float
  val float_to_string : float -> string

  val int_of_float : float -> int
  val float_of_int : int -> float
  
  val int_of_boolean : bool -> int
  val int_to_boolean : int -> bool

  val float_of_boolean : bool -> float
  val float_to_boolean : float -> bool
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

  (** {2 Exceptions} *)

  (** Raised when a table field does not exist. *)
  exception Key_error of string

  (** Raised when a TOML value type is not what an accessor
     or another function expects.
   *)
  exception Type_error of string

  (** Raised when the parser encounters invalid TOML syntax.
     The first member of the tuple is the source file position
     (line and column).
   *)
  exception Parse_error of ((int * int) option * string)

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
      ?newline_before_table:bool -> ?collapse_tables:bool -> ?force_table_arrays:bool ->
      t -> string

    val to_channel :
      ?indent_width:int -> ?indent_character:char -> ?indent_subtables:bool ->
      ?newline_before_table:bool -> ?collapse_tables:bool -> ?force_table_arrays:bool ->
      out_channel -> t -> unit
  end

  module Parser : sig

    (** Reads TOML from a file. May raise {!Parse_error} or {!Stdlib.Sys_error}. *)
    val from_file : string -> t

    (** Reads TOML from an input channel. May raise {!Parse_error} or {!Stdlib.Sys_error}. *)
    val from_channel : in_channel -> t

    (** Reads TOML from a string. May raise {!Parse_error} or {!Stdlib.Sys_error}. *)
    val from_string : string -> t

    (** Like {!from_file}, but handles both {!Parse_error} or {!Stdlib.Sys_error} exceptions
        and wraps the error message in {!Stdlib.result}. *)
    val from_file_result : string -> (t, string) result
    val from_channel_result : in_channel -> (t, string) result
    val from_string_result : string -> (t, string) result

    (** Converts the value attached to a {!Parse_error} exception
        to an error message string.
     *)
    val format_parse_error : (int * int) option -> string -> string
  end

  (** {2 Constructors} *)

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

  (** {2 Accessors} *)

  val get_value : t -> t
  val get_table : t -> (string * t) list
  val get_table_values : (t -> 'a) -> t -> (string * 'a) list

  (** In non-strict mode, forces a value [x] to a single-item array [[x]] *) 
  val get_array : ?strict:bool -> (t -> 'a) -> t -> 'a list

  (** In non-strict mode, converts integer, float, boolean, and datetime values to strings.
      Trying to convert an array or a table to string will raise {!Type_error}.
   *)
  val get_string : ?strict:bool -> t -> string

  (** In non-strict mode, converts string and boolean values to integers.

      Strings are parsed as integers, [true] is converted to 1, [false] is converted to 0,
      and floats are truncated.
   *)
  val get_integer : ?strict:bool -> t -> toml_integer

  (** In non-strict mode, converts string, boolean, and integer values to floats.

   *)
  val get_float : ?strict:bool -> t -> toml_float

  (** In non-strict mode, converts integer, float, and string values to booleans.

      The conversion logic mimics "truth values" in dynamically typed languages.
      Empty strings, numeric values 0 (integer) and 0.0 (float), empty arrays and tables
      are treated as [false], everything else is [true].
   *)
  val get_boolean : ?strict:bool -> t -> bool

  val get_offset_datetime : t -> toml_date
  val get_local_datetime : t -> toml_date
  val get_datetime : t -> toml_date
  val get_local_date : t -> toml_date
  val get_date : t -> toml_date
  val get_local_time : t -> toml_date

  (** Combinators *)

  val get_opt : ('a -> 'b) -> 'a -> 'b option
  val get_result : ('a -> 'b) -> 'a -> ('b, string) result

  (** High-level interface *)

  val path_exists : t -> string list -> bool

  val list_table_keys : t -> string list
  val list_table_keys_exn : t -> string list
  val list_table_keys_result : t -> (string list, string) result

  val find : t -> (t -> 'a) -> string list -> 'a

  val find_exn : t -> (t -> 'a) -> string list -> 'a

  val find_opt : t -> (t -> 'a) -> string list -> 'a option

  val find_or : default:'a -> t -> (t -> 'a) -> string list -> 'a

  val find_result : t -> (t -> 'a) -> string list -> ('a, string) result

  val update : ?use_inline_tables:bool -> t -> string list -> t option -> t

  val update_result : ?use_inline_tables:bool -> t -> string list -> t option -> (t, string) result

  (** Utility functions *)


  (** Makes a printable representation of a table key path,
      for example, [["foo"; "bar baz"; "quux"]] gives [foo."bar baz".quux].
   *)
  val string_of_path : string list -> string
end

