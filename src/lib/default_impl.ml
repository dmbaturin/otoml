module OCamlNumber = struct
  type float = Float.t
  type int = Int.t

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
  let float_of_string x = float_of_string x |> normalize_nan

  (* string_of_float would be sufficient because its outputs
     as of OCaml 4.12 are exactly what TOML uses: nan, inf, -inf

     However, it's probably better to account for possible breaking changes
     in the future, or for default float functions to be shadowed by
     an alternative implementation, than to risk breakage.
   *)
  let float_to_string x =
    if x = infinity then "inf"
    else if x = neg_infinity then "-inf"
    else Printf.sprintf "%.2f" x |> String.lowercase_ascii

  let float_to_boolean x =
    not (x = 0.0)
  let float_of_boolean b =
    if b then 1.0 else 0.0

  let float_of_int = Float.of_int
  let int_of_float = Int.of_float

  let int_of_string = int_of_string
  let int_to_string = Int.to_string

  let int_to_boolean i = i > 0
  let int_of_boolean b = if b then 1 else 0
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
