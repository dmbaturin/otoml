
module OCamlFloat = struct
  type t = Float.t 

  let normalize_nan f =
    (* TOML spec makes no difference between positive and negative NaN,
       but OCaml does, so we strip the sign from NaNs.
     *)
    if Float.is_nan f then Float.copy_sign f 0.0
    else f

  (* float_of_string correctly handles all possible TOML floats,
     including underscores, leading +, the mantissa/exponent format (2021 = 2.021e3),
     and special values: nan, +int, -inf

     It also differentiates between +nan and -nan,
     which is why the normalize function is needed.
   *)
  let of_string x = Stdlib.float_of_string x |> normalize_nan
  let to_string x = Printf.sprintf "%.2f" x

  let to_boolean x = x = 0.0
  let of_boolean b = if b then 1.0 else 0.0
end

module OCamlInteger = struct
  type t = Int.t

  (* int_of_string correctly handles all possible TOML integers,
     including underscores and leading + *)
  let of_string = Stdlib.int_of_string
  let to_string = Int.to_string

  let to_boolean i = i > 0
  let of_boolean b = if b then 1 else 0
end

module StringDate = struct
  type t = string

  let local_time_of_string s = s
  let local_date_of_string s = s
  let local_datetime_of_string s = s
  let offset_datetime_of_string s = s

  let local_time_to_string s = s
  let local_date_to_string s = s
  let local_datetime_to_string s = s
  let offset_datetime_to_string s = s
end
