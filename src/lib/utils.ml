
(* Lists all keys of an assoc list *)
let assoc_keys xs = List.fold_left (fun acc (x, _) -> x :: acc) [] xs

(* Try to nicely handle floats that have a fractional part of zero
   and are essentially integers in disguise. *)
let string_of_float f =
  if f = (Float.round f) then int_of_float f |> string_of_int
  else string_of_float f

let escape_string ?(exclude=[]) s =
  let add_escaped_char buf c =
    let add c = Buffer.add_string buf c in
    (* Since TOML allows multi-line strings that can contain non-escaped
       newlines, line feeds (\r) and tabs,
       we need a way to exclude characters from escaping. *)
    if List.find_opt ((=) c) exclude |> Option.is_some then Buffer.add_char buf c else
    match c with
    | '\\' -> add "\\\\"
    | '"'  -> add "\\\""
    | '\n' -> add "\\n"
    | '\r' -> add "\\r"
    | '\t' -> add "\\t"
    | '\b' -> add "\\b"
    | '\x00' .. '\x1F' | '\x7F' as c ->
      let char_code = Char.code c in
      let char_str =
        (* The TOML spec only allows escapes of the form \uXXXX or \uXXXXXXXX *)
        if char_code > 0xFFFF then Printf.sprintf "%08x" char_code
        else Printf.sprintf "%04x" char_code
      in add char_str
    | _ -> Buffer.add_char buf c
  in
  let buf = Buffer.create 4096 in
  let () = String.iter (add_escaped_char buf) s in
  Buffer.contents buf

let string_of_path ps = String.concat "." ps
