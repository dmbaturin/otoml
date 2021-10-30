open OUnit2

(* The parser is tested with https://github.com/BurntSushi/toml-test
   so we don't add parser tests here. *) 

let toml = Otoml.Parser.from_string "
string_value = \"foo\"
int_value = 42
float_value = 42.0

[table]
value = 1

[table.subtable]
value = \"baz\"
"

let test_find_value _ =
  let res = Otoml.find toml Otoml.get_value ["string_value"] in
  assert_equal res (Otoml.TomlString "foo")

let test_find_value_multi_level _ =
  let res = Otoml.find toml Otoml.get_value ["table"; "subtable"; "value"] in
  assert_equal res (Otoml.TomlString "baz")


let suite =
  "ReaderTests" >::: [
    "test_find_value" >:: test_find_value;
    "test_find_value_multi_level" >:: test_find_value_multi_level;

  ]

let () =
  run_test_tt_main suite
