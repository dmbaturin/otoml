let assoc_keys xs = List.fold_left (fun acc (x, _) -> x :: acc) [] xs

(* Try to nicely handle floats that have a fractional part of zero
   and are essentially integers in disguise. *)
let string_of_float f =
  if f = (Float.round f) then int_of_float f |> string_of_int
  else string_of_float f
