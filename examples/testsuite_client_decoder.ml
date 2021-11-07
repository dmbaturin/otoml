(* This is a client executable for the https://github.com/BurntSushi/toml-test TOML test suite.

   Compile with: ocamlfind ocamlopt -package otoml,zarith,decimal,yojson -linkpkg ./examples/testsuite_client_decoder.ml -o decoder
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

  (* Decimal.to_string uses "NaN" spelling
     while TOML requires all special float values to be lowercase. *)
  let float_to_string x = Decimal.to_string x |> String.lowercase_ascii
  let float_of_boolean b = if b then Decimal.one else Decimal.zero
  let float_to_boolean x = (x <> Decimal.zero)

  let float_of_int = Decimal.of_bigint
  let int_of_float = Decimal.to_bigint
end

module Otoml = Otoml.Base.Make (BigNumber) (Otoml.Base.StringDate)

open Otoml

let type_string v =
  match v with
  | TomlString _ -> "string"
  | TomlInteger _ -> "integer"
  | TomlFloat _ -> "float"
  | TomlBoolean _ -> "bool"
  | TomlLocalTime _ -> "time-local"
  | TomlLocalDate _ -> "date-local"
  | TomlLocalDateTime _ -> "datetime-local"
  | TomlOffsetDateTime _ -> "datetime"
  (* Not actually needed here since the testsuite runner
     expects arrays and tables to become normal JSON lists and objects. *)
  | TomlArray _ -> "array"
  | TomlTable _ | TomlInlineTable _ | TomlTableArray _ -> "table"

let rec to_json t =
  match t with
  | TomlTable kvs ->
    `Assoc (List.map (fun (k, v) -> (k, json_of_value v)) kvs)
  | TomlInlineTable kvs ->
    `Assoc (List.map (fun (k, v) -> (k, json_of_value v)) kvs)
  | TomlArray vs ->
    `List (List.map json_of_value vs)
  | TomlTableArray vs ->
    `List (List.map json_of_value vs)
  | _ -> failwith "bad type"
and json_of_value v =
  let typ = ("type", `String (type_string v)) in
  match v with
  | TomlInteger i -> `Assoc [typ; "value", `String (BigNumber.int_to_string i)]
  | TomlFloat f -> `Assoc [typ; "value", `String (BigNumber.float_to_string f)]
  | TomlString s -> `Assoc [typ; "value", `String (s)]
  | TomlBoolean b -> `Assoc [typ; "value", `String (string_of_bool b)]
  | TomlLocalTime s -> `Assoc [typ; "value", `String (s)]
  | TomlLocalDate s -> `Assoc [typ; "value", `String (s)]
  | TomlLocalDateTime s -> `Assoc [typ; "value", `String (s)]
  | TomlOffsetDateTime s -> `Assoc [typ; "value", `String (s)]
  | TomlTable _ as t -> to_json t
  | TomlInlineTable _ as t -> to_json t
  | TomlArray _ as a -> to_json a
  | TomlTableArray _ as a -> to_json a

let () =
  let res = Parser.from_channel_result stdin in
  match res with
  | Ok toml ->
    let json = to_json toml in
    Yojson.to_channel stdout json
  | Error err ->
    Printf.eprintf "%s" err;
    exit 1
