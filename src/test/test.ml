open OUnit2

module OT = Otoml

(* The parser is tested with https://github.com/BurntSushi/toml-test
   so we don't add parser tests here. *) 

let toml = Otoml.Parser.from_string {|
string_value = "foo"
int_value = 42
float_value = 42.0

[table]
value = 1

[table.subtable]
value = "baz"
|}

(* High level interface tests *)

let test_find_value _ =
  let res = OT.find toml OT.get_value ["string_value"] in
  assert_equal res (OT.TomlString "foo")

let test_find_value_multi_level _ =
  let res = OT.find toml OT.get_value ["table"; "subtable"; "value"] in
  assert_equal res (OT.TomlString "baz")

let test_find_value_nonexistent_exn _ =
  try
    let _ = OT.find toml OT.get_value ["no"; "such"; "key"] in
    assert_failure "Trying to retrieve a non-existent key succeeded"
  with OT.Key_error _ -> ()

(* Node update tests *)

let test_add_field _ =
  let new_toml = OT.update toml ["table"; "subtable"; "new_value"] (Some (OT.integer 92)) in
  let res = OT.find new_toml OT.get_integer ["table"; "subtable"; "new_value"] in
  assert_equal res 92

let test_update_field _ =
  let new_toml = OT.update toml ["table"; "subtable"; "value"] (Some (OT.integer 92)) in
  let res = OT.find new_toml OT.get_integer ["table"; "subtable"; "value"] in
  assert_equal res 92

let test_delete_field _ =
  let new_toml = OT.update toml ["table"; "subtable"; "value"] None in
  assert_bool "field should not exist" (not (OT.path_exists new_toml ["table"; "subtable"; "value"]))


let suite =
  "TOMLTests" >::: [
    "test_find_value" >:: test_find_value;
    "test_find_value_multi_level" >:: test_find_value_multi_level;
    "test_find_value_nonexistent_exn" >:: test_find_value_nonexistent_exn;
    "test_add_field" >:: test_add_field;
    "test_update_field" >:: test_update_field;
    "test_delete_field" >:: test_delete_field;
  ]

let () =
  run_test_tt_main suite
