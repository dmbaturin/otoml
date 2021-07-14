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
