(* This is a client executable for the https://github.com/BurntSushi/toml-test TOML test suite.

   Compile with: ocamlfind ocamlopt -package otoml,zarith,decimal,yojson -linkpkg ./examples/testsuite_client_encoder.ml -o encoder
 *)

(* No signature ascriptions: something like
   `module BigNumber : Otoml.Base.TomlNumber` would make the type t abstract,
   which is inconvenient.
   There are two ways to avoid that: either write `with type` explicitly,
   or just omit the ascription.
 *)
module BigNumber = struct
  type int = Z.t
  type float = Decimal.t

  let int_of_string = Z.of_string
  let int_to_string = Z.to_string
  let int_of_boolean b = if b then Z.one else Z.zero
  let int_to_boolean n = (n <> Z.zero)

  (* Can't just reuse Decimal.to/of_string because their optional arguments
     would cause a signature mismatch. *)
  let float_of_string s = Decimal.of_string s

  (* Converting Decimal.t to a TOML value string takes some fixups.

     First, Decimal.to_string uses "NaN" spelling
     while TOML requires all special float values to be lowercase.

     Second, it uses "Infinity" rather than "inf" for infinite numbers,
     while TOML requires "inf".
   *)
  let float_to_string f =
    if f = Decimal.infinity then "inf"
    else if f = Decimal.neg_infinity then "-inf"
    else Decimal.to_string f |> String.lowercase_ascii

  let float_of_boolean b = if b then Decimal.one else Decimal.zero
  let float_to_boolean x = (x <> Decimal.zero)

  let float_of_int = Decimal.of_bigint
  let int_of_float = Decimal.to_bigint

end


module Otoml = Otoml.Base.Make (BigNumber) (Otoml.Base.StringDate)

module OT = Otoml

let rec from_json j =
  match j with
  | `Assoc [("type", `String t); ("value", `String v)] ->
    begin match t with
    | "string" -> OT.string v
    | "integer" -> OT.integer (Z.of_string v)
    | "float" -> OT.float (Decimal.of_string v)
    | "bool" -> OT.boolean (bool_of_string v)
    | "datetime" -> OT.offset_datetime v
    | "datetime-local" -> OT.local_datetime v
    | "date-local" -> OT.local_date v
    | "time-local" -> OT.local_time v
    | _ -> Printf.ksprintf failwith "Unknown value type \"%s\"" t
    end
  | `Assoc vs -> Otoml.table (List.map (fun (k, v) -> (k, from_json v)) vs)
  | `List vs -> Otoml.array (List.map from_json vs)
  | _ -> failwith "Unexpected JSON input"

let () =
  let json = Yojson.Safe.from_channel stdin in
  let toml = from_json json in
  Otoml.Printer.to_channel ~force_table_arrays:true stdout toml  
