(* This is a client executable for the https://github.com/BurntSushi/toml-test TOML test suite.

   Compile with: ocamlfind ocamlopt -package otoml,zarith,decimal,yojson -linkpkg ./examples/testsuite_client_encoder.ml -o encoder
 *)

module BigInteger = struct
  type t = Z.t
  let of_string = Z.of_string
  let to_string = Z.to_string
  let of_boolean b = if b then Z.one else Z.zero
  let to_boolean n = (n <> Z.zero)
end

module BigFloat = struct
  type t = Decimal.t
  (* Can't just reuse Decimal.to/of_string because their optional arguments
     would cause a signature mismatch. *)
  let of_string s = Decimal.of_string s

  (* Decimal.to_string uses "NaN" spelling
     while TOML requires all special float values to be lowercase. *)
  let to_string x = Decimal.to_string x |> String.lowercase_ascii
  let of_boolean b = if b then Decimal.one else Decimal.zero
  let to_boolean x = (x <> Decimal.zero)
end

module Otoml = Otoml.Base.Make (BigInteger) (BigFloat) (Otoml.Base.StringDate)

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
