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

  (** Raised when a table field or a table name is defined twice,
      which is prohibited to the TOML standard. *)
  exception Duplicate_key of string

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

  (** {2 Constructors}

      Constructors create TOML values from OCaml values.
   *)

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

  (** {2 Accessors}

      Accessors can be used by themselves on TOML values, or passed to
      high-level interface functions such as {!find}.

      By default they expect a strict match, e.g. {!get_string}
      fails on values other than [TomlString _]. However, they all
      provide a boolean [~strict] argument that enables type conversion when set to [false].
      Not all types can be converted between each other, so [~strict:false]
      does not prevent all type errors.
 
      All accessors will raise {!Type_error} exception if type conversion
      is disabled or fails. High-level interface functions handle those exceptions,
      so you don't need to handle it.
      
      If you want to use accessors directly on TOML values and you want option or result
      values instead of exceptions, you can use {!get_opt} and {!get_result} combinators.
   *)

  (** The trivial accessor that returns the unwrapped TOML value. *)
  val get_value : t -> t

  val get_table : t -> (string * t) list

  (** Unwraps a table and applies an accessor to the values of its fields,
      useful for unwrapping tables with homogenous field types in a single step. *)
  val get_table_values : (t -> 'a) -> t -> (string * 'a) list

  (** Converts a TOML array to a list of OCaml values by applying
      an accessor function to each of them.

      For example, if you want to retrieve an array of strings,
      you can use [get_array get_string toml_value].

      In non-strict mode, forces a value [x] to a single-item array [[x]].

      Note that the [strict] flag is not passed to the accessor.
      If you want the accessor to also attempt type conversion on the array values,
      you should specify it explicitly:
      [get_array ~strict:false (get_string ~strict:false) toml_value].
   *) 
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

  (** In non-strict mode, these functions will try to convert strings to dates.

      In the default implementation dates are represented as strings,
      so the conversion is a no-op.

      They will handle the {!Stdlib.Failure} exception raised by string to datetime
      conversion functions. Thus if you supply your own datetime module
      to the functorial interface, you may want to catch exceptions raised by your
      library of choice and re-raise them as [Failure].
   *)
  val get_offset_datetime : ?strict:bool -> t -> toml_date
  val get_local_datetime : ?strict:bool -> t -> toml_date
  val get_local_date : ?strict:bool -> t -> toml_date
  val get_local_time : ?strict:bool -> t -> toml_date

  val get_datetime : t -> toml_date
  val get_date : t -> toml_date

  (** {2 Combinators }

      These combinators are mainly useful for unwrapping standalong TOML values by hand.
      They handle the {!Type_error} exception and return [None] or [Error msg] when it occurs.

      The high-level interface functions handle exceptions raised by accessors themselves.
   *)

  val get_opt : ('a -> 'b) -> 'a -> 'b option
  val get_result : ('a -> 'b) -> 'a -> ('b, string) result

  (** {2 High-level interface} *)

  (** Returns [true] if there is a value at the specified path in a table.

      For the purpose of this function, an empty table does exist,
      i.e. if you have [foo.bar = {}], then [path_exists t ["foo"; "bar"]] is true.
   *)
  val path_exists : t -> string list -> bool

  (** Returns a list of all keys of a table, in their original order.

      @raises {!Type_error} is the value is not a table.
   *)
  val list_table_keys : t -> string list

  val list_table_keys_exn : t -> string list

  val list_table_keys_result : t -> (string list, string) result

  (** Looks up a value in a table and unwraps it using an accessor function.

      @raises {!Key_error} if there's no such key path in the table.

      @raises {!Type_error} if the value itself is not a table or the field value
      is not what the accessor expects.
   *)
  val find : t -> (t -> 'a) -> string list -> 'a

  val find_exn : t -> (t -> 'a) -> string list -> 'a

  val find_opt : t -> (t -> 'a) -> string list -> 'a option

  val find_or : default:'a -> t -> (t -> 'a) -> string list -> 'a

  val find_result : t -> (t -> 'a) -> string list -> ('a, string) result

  (** Updates a table field at a specified path.

      Passing [Some toml_value] inserts a new value or replaces an existing value.

      If a key path partially does not exist, additional tables are created as needed.
      For example, [update (TomlTable []) ["foo"; "bar"] (Some (TomlString "baz"]))]
      will produce [TomlTable [("foo", TomlTable [("bar", TomlString "baz")])]].

      The [use_inline_tables] flag determines whether automatically-created missing tables
      will be normal or inline tables.

      Passing [None] as the argument will delete the field at the specified path.
      It's safe to attempt deleting values at paths that don't exist:
      there will be no error and the original TOML will be returned unchanged.
   *)
  val update : ?use_inline_tables:bool -> t -> string list -> t option -> t

  val update_result : ?use_inline_tables:bool -> t -> string list -> t option -> (t, string) result

  (** {3 Utility functions} *)


  (** Makes a printable representation of a table key path,
      for example, [["foo"; "bar baz"; "quux"]] gives [foo."bar baz".quux].
   *)
  val string_of_path : string list -> string

  module Helpers : sig
    (** {4 string value lookup} *)

    (** Looks up an string at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an string
        or (when used with [~strict:false], if it cannot be converted to string.
     *)
    val find_string : ?strict:bool -> t -> string list -> string

    (** Alias for {!find_string} *)
    val find_string_exn : ?strict:bool -> t -> string list -> string

    (** Looks up an string at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an string
        or (when used with [~strict:false], if it cannot be converted to string.
     *)
    val find_string_opt : ?strict:bool -> t -> string list -> string option

    (** Looks up an string at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an string nor (with [~strict:false])
        a value that can be converted to string.
     *)
    val find_string_result : ?strict:bool -> t -> string list -> (string, string) result

    (** {4 string array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not an string, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single string value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of strings at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of strings
        or (when used with [~strict:false], if it cannot be converted to an array of strings.
     *)
    val find_strings : ?strict:bool -> t -> string list -> string list

    (** Alias for {!find_strings} *)
    val find_strings_exn : ?strict:bool -> t -> string list -> string list

    (** Looks up an array of strings at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an string or an array of strings
        or (when used with [~strict:false], if it cannot be converted to an array of strings.
     *)
    val find_strings_opt : ?strict:bool -> t -> string list -> string list option

    (** Looks up an array of strings at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an string or an array of strings, nor (with [~strict:false])
        a value that can be converted to an array of strings.
     *)
    val find_strings_result : ?strict:bool -> t -> string list -> (string list, string) result


    (** {4 Integer value lookup} *)

    (** Looks up an integer at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an integer
        or (when used with [~strict:false], if it cannot be converted to integer.
     *)
    val find_integer : ?strict:bool -> t -> string list -> toml_integer

    (** Alias for {!find_integer} *)
    val find_integer_exn : ?strict:bool -> t -> string list -> toml_integer

    (** Looks up an integer at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an integer
        or (when used with [~strict:false], if it cannot be converted to integer.
     *)
    val find_integer_opt : ?strict:bool -> t -> string list -> toml_integer option

    (** Looks up an integer at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an integer nor (with [~strict:false])
        a value that can be converted to integer.
     *)
    val find_integer_result : ?strict:bool -> t -> string list -> (toml_integer, string) result

    (** {4 Integer array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not an integer, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single integer value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of integers at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of integers
        or (when used with [~strict:false], if it cannot be converted to an array of integers.
     *)
    val find_integers : ?strict:bool -> t -> string list -> toml_integer list

    (** Alias for {!find_integers} *)
    val find_integers_exn : ?strict:bool -> t -> string list -> toml_integer list

    (** Looks up an array of integers at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an integer or an array of integers
        or (when used with [~strict:false], if it cannot be converted to an array of integers.
     *)
    val find_integers_opt : ?strict:bool -> t -> string list -> toml_integer list option

    (** Looks up an array of integers at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an integer or an array of integers, nor (with [~strict:false])
        a value that can be converted to an array of integers.
     *)
    val find_integers_result : ?strict:bool -> t -> string list -> (toml_integer list, string) result

    (** {4 float value lookup} *)

    (** Looks up an float at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an float
        or (when used with [~strict:false], if it cannot be converted to float.
     *)
    val find_float : ?strict:bool -> t -> string list -> toml_float

    (** Alias for {!find_float} *)
    val find_float_exn : ?strict:bool -> t -> string list -> toml_float

    (** Looks up an float at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an float
        or (when used with [~strict:false], if it cannot be converted to float.
     *)
    val find_float_opt : ?strict:bool -> t -> string list -> toml_float option

    (** Looks up an float at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an float nor (with [~strict:false])
        a value that can be converted to float.
     *)
    val find_float_result : ?strict:bool -> t -> string list -> (toml_float, string) result

    (** {4 float array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not an float, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single float value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of floats at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of floats
        or (when used with [~strict:false], if it cannot be converted to an array of floats.
     *)
    val find_floats : ?strict:bool -> t -> string list -> toml_float list

    (** Alias for {!find_floats} *)
    val find_floats_exn : ?strict:bool -> t -> string list -> toml_float list

    (** Looks up an array of floats at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an float or an array of floats
        or (when used with [~strict:false], if it cannot be converted to an array of floats.
     *)
    val find_floats_opt : ?strict:bool -> t -> string list -> toml_float list option

    (** Looks up an array of floats at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an float or an array of floats, nor (with [~strict:false])
        a value that can be converted to an array of floats.
     *)
    val find_floats_result : ?strict:bool -> t -> string list -> (toml_float list, string) result

    (** {4 boolean value lookup} *)

    (** Looks up a boolean at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not a boolean
        or (when used with [~strict:false], if it cannot be converted to boolean.
     *)
    val find_boolean : ?strict:bool -> t -> string list -> bool

    (** Alias for {!find_boolean} *)
    val find_boolean_exn : ?strict:bool -> t -> string list -> bool

    (** Looks up a boolean at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a boolean
        or (when used with [~strict:false], if it cannot be converted to boolean.
     *)
    val find_boolean_opt : ?strict:bool -> t -> string list -> bool option

    (** Looks up a boolean at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a boolean nor (with [~strict:false])
        a value that can be converted to boolean.
     *)
    val find_boolean_result : ?strict:bool -> t -> string list -> (bool, string) result

    (** {4 boolean array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not a boolean, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single boolean value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of booleans at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of booleans
        or (when used with [~strict:false], if it cannot be converted to an array of booleans.
     *)
    val find_booleans : ?strict:bool -> t -> string list -> bool list

    (** Alias for {!find_booleans} *)
    val find_booleans_exn : ?strict:bool -> t -> string list -> bool list

    (** Looks up an array of booleans at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a boolean or an array of booleans
        or (when used with [~strict:false], if it cannot be converted to an array of booleans.
     *)
    val find_booleans_opt : ?strict:bool -> t -> string list -> bool list option

    (** Looks up an array of booleans at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a boolean or an array of booleans, nor (with [~strict:false])
        a value that can be converted to an array of booleans.
     *)
    val find_booleans_result : ?strict:bool -> t -> string list -> (bool list, string) result

    (** {4 offset_datetime value lookup} *)

    (** Looks up an offset_datetime at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an offset datetime
        or (when used with [~strict:false], if it cannot be converted to offset datetime.
     *)
    val find_offset_datetime : ?strict:bool -> t -> string list -> toml_date

    (** Alias for {!find_offset_datetime} *)
    val find_offset_datetime_exn : ?strict:bool -> t -> string list -> toml_date

    (** Looks up an offset_datetime at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an offset datetime
        or (when used with [~strict:false], if it cannot be converted to offset datetime.
     *)
    val find_offset_datetime_opt : ?strict:bool -> t -> string list -> toml_date option

    (** Looks up an offset_datetime at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an offset_datetime nor (with [~strict:false])
        a value that can be converted to offset_datetime.
     *)
    val find_offset_datetime_result : ?strict:bool -> t -> string list -> (toml_date, string) result

    (** {4 offset_datetime array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not an offset datetime, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single offset_datetime value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of offset_datetimes at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of offset_datetimes
        or (when used with [~strict:false], if it cannot be converted to an array of offset_datetimes.
     *)
    val find_offset_datetimes : ?strict:bool -> t -> string list -> toml_date list

    (** Alias for {!find_offset_datetimes} *)
    val find_offset_datetimes_exn : ?strict:bool -> t -> string list -> toml_date list

    (** Looks up an array of offset_datetimes at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not an offset datetime or an array of offset datetimes
        or (when used with [~strict:false], if it cannot be converted to an array of offset datetimes.
     *)
    val find_offset_datetimes_opt : ?strict:bool -> t -> string list -> toml_date list option

    (** Looks up an array of offset_datetimes at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither an offset datetime or an array of offset datetimes, nor (with [~strict:false])
        a value that can be converted to an array of offset_datetimes.
     *)
    val find_offset_datetimes_result : ?strict:bool -> t -> string list -> (toml_date list, string) result

    (** {4 local_datetime value lookup} *)

    (** Looks up a local_datetime at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local datetime
        or (when used with [~strict:false], if it cannot be converted to local datetime.
     *)
    val find_local_datetime : ?strict:bool -> t -> string list -> toml_date

    (** Alias for {!find_local_datetime} *)
    val find_local_datetime_exn : ?strict:bool -> t -> string list -> toml_date

    (** Looks up a local_datetime at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local datetime
        or (when used with [~strict:false], if it cannot be converted to local datetime.
     *)
    val find_local_datetime_opt : ?strict:bool -> t -> string list -> toml_date option

    (** Looks up a local_datetime at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a local datetime nor (with [~strict:false])
        a value that can be converted to local datetime.
     *)
    val find_local_datetime_result : ?strict:bool -> t -> string list -> (toml_date, string) result

    (** {4 local_datetime array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not a local datetime, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single local_datetime value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of local_datetimes at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of local datetimes
        or (when used with [~strict:false], if it cannot be converted to an array of local datetimes.
     *)
    val find_local_datetimes : ?strict:bool -> t -> string list -> toml_date list

    (** Alias for {!find_local_datetimes} *)
    val find_local_datetimes_exn : ?strict:bool -> t -> string list -> toml_date list

    (** Looks up an array of local_datetimes at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local datetime or an array of local datetimes
        or (when used with [~strict:false], if it cannot be converted to an array of local_datetimes.
     *)
    val find_local_datetimes_opt : ?strict:bool -> t -> string list -> toml_date list option

    (** Looks up an array of local_datetimes at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a local datetime or an array of local datetimes, nor (with [~strict:false])
        a value that can be converted to an array of local_datetimes.
     *)
    val find_local_datetimes_result : ?strict:bool -> t -> string list -> (toml_date list, string) result

    (** {4 local_date value lookup} *)

    (** Looks up a local_date at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local_date
        or (when used with [~strict:false], if it cannot be converted to local_date.
     *)
    val find_local_date : ?strict:bool -> t -> string list -> toml_date

    (** Alias for {!find_local_date} *)
    val find_local_date_exn : ?strict:bool -> t -> string list -> toml_date

    (** Looks up a local_date at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local_date
        or (when used with [~strict:false], if it cannot be converted to local_date.
     *)
    val find_local_date_opt : ?strict:bool -> t -> string list -> toml_date option

    (** Looks up a local_date at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a local_date nor (with [~strict:false])
        a value that can be converted to local_date.
     *)
    val find_local_date_result : ?strict:bool -> t -> string list -> (toml_date, string) result

    (** {4 local_date array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not a local date, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single local_date value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of local_dates at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of local dates
        or (when used with [~strict:false], if it cannot be converted to an array of local dates.
     *)
    val find_local_dates : ?strict:bool -> t -> string list -> toml_date list

    (** Alias for {!find_local_dates} *)
    val find_local_dates_exn : ?strict:bool -> t -> string list -> toml_date list

    (** Looks up an array of local_dates at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local date or an array of local dates
        or (when used with [~strict:false], if it cannot be converted to an array of local dates.
     *)
    val find_local_dates_opt : ?strict:bool -> t -> string list -> toml_date list option

    (** Looks up an array of local_dates at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a local_date or an array of local_dates, nor (with [~strict:false])
        a value that can be converted to an array of local_dates.
     *)
    val find_local_dates_result : ?strict:bool -> t -> string list -> (toml_date list, string) result

    (** {4 local_time value lookup} *)

    (** Looks up a local_time at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local_time
        or (when used with [~strict:false], if it cannot be converted to local time.
     *)
    val find_local_time : ?strict:bool -> t -> string list -> toml_date

    (** Alias for {!find_local_time} *)
    val find_local_time_exn : ?strict:bool -> t -> string list -> toml_date

    (** Looks up a local_time at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local time
        or (when used with [~strict:false], if it cannot be converted to local_time.
     *)
    val find_local_time_opt : ?strict:bool -> t -> string list -> toml_date option

    (** Looks up a local_time at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a local time nor (with [~strict:false])
        a value that can be converted to local_time.
     *)
    val find_local_time_result : ?strict:bool -> t -> string list -> (toml_date, string) result

    (** {4 local_time array value lookup

        Note: for all functions that look up arrays, [~strict:true] applies only to array item conversion.

        With [~strict:true] they will fail if an array contains a value that is not a local time, while with [~strict:false]
        it will attempt to convert values (i.e. pass [~strict:false] to the accessor function).

        If a user supplies a single local time value instead of an array, it will always be converted to a single-item array.
     *)

    (** Looks up an array of local_times at specified path.

        @raises {!Key_error} if there's no such key path in the table.

        @raises {!Type_error} if the value itself is not a table or the field value is not an array of local times
        or (when used with [~strict:false], if it cannot be converted to an array of local times.
     *)
    val find_local_times : ?strict:bool -> t -> string list -> toml_date list

    (** Alias for {!find_local_times} *)
    val find_local_times_exn : ?strict:bool -> t -> string list -> toml_date list

    (** Looks up an array of local_times at specified path, returns [None] if path does not exist.

        @raises {!Type_error} if the value itself is not a table or the field value is not a local time or an array of local times
        or (when used with [~strict:false], if it cannot be converted to an array of local_times.
     *)
    val find_local_times_opt : ?strict:bool -> t -> string list -> toml_date list option

    (** Looks up an array of local_times at specified path, returns an [Error] with a descriptive message
        if path does not exist or the value is neither a local time or an array of local_times, nor (with [~strict:false])
        a value that can be converted to an array of local times.
     *)
    val find_local_times_result : ?strict:bool -> t -> string list -> (toml_date list, string) result

  end
end

