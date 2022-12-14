(* Errors *)

exception Key_error of string
exception Type_error of string
exception Parse_error of ((int * int) option * string)
exception Duplicate_key of string

(* Convenience functions for throwing exceptions *)
let key_error err = raise (Key_error err)
let type_error err = raise (Type_error err)
let duplicate_key_error err = raise (Duplicate_key err)
