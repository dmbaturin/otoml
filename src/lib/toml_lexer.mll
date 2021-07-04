(*
 * Copyright (c) 2021 Daniil Baturin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

{

open Parser_utils
open Toml_parser

let lexing_error lexbuf msg =
  let line, column = Parser_utils.get_lexing_position lexbuf in
  raise (Parse_error (Some (line, column), msg))

exception Bad_unicode of (string * int)

let validate_unicode lexbuf s =
  let validate _ column character =
    match character with
    | `Malformed s -> raise (Bad_unicode (s, column))
    | _ -> ()
  in
  try Uutf.String.fold_utf_8 validate () s
  with Bad_unicode (c, column) ->
    let line, _ = Parser_utils.get_lexing_position lexbuf in
    let msg = Printf.sprintf "malformed UTF-8 character \"%s\" on line %d (column %d within the string or comment)"
      c line column
    in raise (Parse_error (None, msg))

(* "Date validation".

    The goal is to reject completely implausible dates,
    since deep validation (if the user wants to work with dates to begin with)
    can and should be done by a real calendar library.

    Exception handling is added just in case.
    In practice invalid integers shouldn't make it that far so int_of_string should work,
    and int_of_string is the only function that can fail there,
    but it's probably better to be on the safe side.
 *)
let valid_time hours minutes seconds =
  try
    ((int_of_string hours) <= 23) &&
    ((int_of_string minutes) <= 59) &&
    ((int_of_string seconds) <= 60) (* Leap second is a real thing. *)
  with _ -> false

let valid_date year month day =
  try
    let year, month, day =
      int_of_string year, int_of_string month, int_of_string day
    in
    (year >= 1) &&
    ((month >= 1) && (month <= 12)) &&
    (day >= 1) &&
    (if month = 2 then (day <= 29) else (day <= 31))
    (* || ((year = 1993) && (month = 9)) *)
  with _ -> false

let valid_timezone hours minutes =
  match (hours, minutes) with
  | Some hours, Some minutes -> begin
      try
        ((int_of_string hours) <= 23) &&
        ((int_of_string minutes) <= 59)
      with _ -> false
    end
  | _, _ ->
    (* They can only be both Some or both None.
       The latter happens when a datetime uses "Z" instead of a timezone,
       in that case the timezone doesn't need checking.
     *)
    true

let add_utf8_char lexbuf buf num_s =
  try
    let num = int_of_string ("0x" ^ num_s) in
    let uc = Uchar.of_int num in
    Buffer.add_utf_8_uchar buf uc
  with Invalid_argument _ | Failure _ ->
    lexing_error lexbuf @@ Printf.sprintf "\\u%s is not a valid UTF-8 character escape" num_s

let trim_left_newline s =
  let len = String.length s in
  if len = 0 then s else
  let first_char = String.get s 0 in
  match first_char with
  | '\n' -> String.sub s 1 (len - 1)
  | '\r' ->
    (* Is it a file with Windows newlines? *)
    if len > 1 then begin
      match (String.get s 1) with
      | '\n' -> String.sub s 2 (len - 1)
      | _ ->
        (* Better not to think why a string starts with a \r then. *)
        s
    end
    else s
  | _ -> s

let newlines lexbuf s =
  let newline lexbuf c =
    match c with
    | '\n' -> Lexing.new_line lexbuf
    | _ -> ()
  in String.iter (newline lexbuf) s

let move_position lexbuf n =
  let open Lexing in
  lexbuf.lex_curr_pos <- (lexbuf.lex_curr_pos + n)
}

(** Reusable numeric regexes *)

let t_digit = ['0'-'9']
let t_sign = ['+' '-']

(* Numbers must not have leading zeroes.
   Digits may be separated by a single underscore.
 *)
let t_integer_part  = '0' | ['1'-'9'] ('_'? t_digit+)*

(* >An exponent part is an E (upper or lower case) followed by an integer part
   >(which follows the same rules as decimal integer values but may include leading zeros).
 *)
let t_exponent = ['E' 'e'] t_sign? '0'* t_integer_part

(* This covers decimals (42, +42, -42)
   and prefixed base-2/8/16 integers (0xFF, 0o54, 0xDEAD_BEEF...)

   As the spec says:
   "Non-negative integer values may also be expressed in hexadecimal, octal, or binary.
    In these formats, leading + is not allowed and leading zeros are allowed (after the prefix)"
 *)

let t_bin_digit = ['0'-'1']
let t_bin_integer_part = t_bin_digit ('_'? t_bin_digit+)*

let t_oct_digit = ['0'-'7']
let t_oct_integer_part = t_oct_digit ('_'? t_oct_digit+)*

let t_hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let t_hex_integer_part = t_hex_digit ('_'? t_hex_digit+)*

let t_integer =
  t_sign? t_integer_part 
| "0b" '0'* t_bin_integer_part
| "0o" '0'* t_oct_integer_part
| "0x" '0'* t_hex_integer_part

(* Numbers with an exponent are always interpreted as floats.
   The spec disallows floats with an implicit integer/fractional part, like 42. and .42,
   so we don't cover those cases.
  *)
let t_fractional_part = '.' t_digit ('_'? t_digit+)*
let t_float_number = t_sign? t_integer_part ((t_fractional_part t_exponent?) | t_exponent)

(* >Special float values can also be expressed. They are always lowercase.

  Conveniently, the float_of_string function allows every special value string required by TOML:
  nan, +nan, -nan, inf, +inf, -inf

  +nan and -nan are both interpreted as just a nan

 *)
let t_float = t_float_number | (t_sign? ("nan" | "inf"))

(* Date and time *)

let t_time =
  (t_digit t_digit as hours)   ':'
  (t_digit t_digit as minutes) ':'
  (t_digit t_digit as seconds)
  ('.' t_digit+)?

(* Timezone part: either Z/z (Zulu time = UTC) or offset: +05:45, -08:00... *)

let t_timezone =
  ('Z' | 'z') | (t_sign (t_digit t_digit as tz_hours) ':' (t_digit t_digit as tz_minutes))

(* Date part: 1970-01-01 *)
let t_date =
  (t_digit t_digit t_digit t_digit as year) '-'
  (t_digit t_digit as month)                '-'
  (t_digit t_digit as day)

(* For the sake of readability, you may replace the T delimiter between date and time with a space character
   (as permitted by RFC 3339 section 5.6).

   RFC 3339 explcitly allows lowercase 't' and 'z':
   > NOTE: Per [ABNF] and ISO8601, the "T" and "Z" characters in this
   > syntax may alternatively be lower case "t" or "z" respectively.
 *)
let t_local_datetime = t_date ('T' | 't' | ' ') t_time
let t_offset_datetime = t_date ('T' | 't' | ' ') t_time t_timezone


(* Unicode escape sequences, for \uXXXX and \uXXXXXXXX. *)
let t_unicode =
  (t_hex_digit t_hex_digit t_hex_digit t_hex_digit) |
  (t_hex_digit t_hex_digit t_hex_digit t_hex_digit
   t_hex_digit t_hex_digit t_hex_digit t_hex_digit)

rule token = parse
(* Whitespace *)
| ('\n' | '\r' '\n') { Lexing.new_line lexbuf; NEWLINE }
| [' ' '\t']
  { token lexbuf }
(* Punctuation *)
| "=" { EQ }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' '[' { LDBRACKET }
| ']' ']' { RDBRACKET }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '.' { DOT }
| ',' { COMMA }
(* Primitive values *)
| t_time as t
  {
    if valid_time hours minutes seconds then LOCAL_TIME(t) else
    lexing_error lexbuf @@ Printf.sprintf "%s is not a valid time" t
  }
| t_date as d
  { if valid_date year month day then LOCAL_DATE(d) else
    lexing_error lexbuf @@ Printf.sprintf "%s is not a valid date" d
  }
| t_local_datetime as dt
  {
    if (valid_date year month day) && (valid_time hours minutes seconds) then LOCAL_DATETIME(dt) else
    lexing_error lexbuf @@ Printf.sprintf "%s is not a valid local datetime" dt
  }
| t_offset_datetime as dt
  {
    if (valid_date year month day) && (valid_time hours minutes seconds) && (valid_timezone tz_hours tz_minutes)
    then OFFSET_DATETIME(dt)
    else lexing_error lexbuf @@ Printf.sprintf "%s is not a valid datetime" dt
  }
| t_integer as s
  (* int_of_string correctly handles all possible TOML integers,
     including underscores and leading + *)
  { INTEGER(int_of_string s) }
| t_float as s
  (* float_of_string also covers all notations valid in TOML *)
  { FLOAT(float_of_string s) }
| ("true" | "false") as s
  (* Boolean literals must always be lowercase in TOML. *)
  { BOOLEAN(bool_of_string s) }
(* Bare keys. CAUTION: this _must_ come after primitive values
   because integers and booleans match the same regex! *)
| ['A'-'Z''a'-'z''0'-'9''_''-']+ as s { KEY(s) }
| "'''''"
  { let buf = Buffer.create 512 in Buffer.add_string buf "''"; read_single_quoted_multiline_string buf lexbuf }
| "''''"
  { let buf = Buffer.create 512 in Buffer.add_string buf "'"; read_single_quoted_multiline_string buf lexbuf }
| "'''"
    { read_single_quoted_multiline_string (Buffer.create 512) lexbuf }
| '"' '"' '"' '"' '"'
  { let buf = Buffer.create 512 in Buffer.add_string buf "\"\""; read_double_quoted_multiline_string buf lexbuf }
| '"' '"' '"' '"'
  { let buf = Buffer.create 512 in Buffer.add_string buf "\""; read_double_quoted_multiline_string buf lexbuf }
| '"' '"' '"'
    { read_double_quoted_multiline_string (Buffer.create 512) lexbuf }
| '''
    { read_single_quoted_string (Buffer.create 512) lexbuf }
| '"'
    { read_double_quoted_string (Buffer.create 512) lexbuf }
| '#'
    { read_comment (Buffer.create 512) lexbuf; Lexing.new_line lexbuf; token lexbuf }
| eof { EOF }
| _ as bad_char
  { lexing_error lexbuf (Printf.sprintf "unexpected character \'%s\'" (Char.escaped bad_char)) }

and read_comment buf =
  parse
  | ['\x00'-'\x08' '\x0B'-'\x1F' '\x7F'] as bad_char
      {
        lexing_error lexbuf @@
        Printf.sprintf "character '%s' is not allowed inside a comment"
        (Char.escaped bad_char)
      }
  | ('\n' | '\r' '\n') { validate_unicode lexbuf @@ Buffer.contents buf }
  | [^ '\n' '\x00'-'\x08' '\x0B'-'\x1F' '\x7F']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_comment buf lexbuf }

and read_double_quoted_string buf =
  parse
  | '"'       { validate_unicode lexbuf @@ Buffer.contents buf; STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_double_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_double_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_double_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_double_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_double_quoted_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_double_quoted_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_double_quoted_string buf lexbuf }
  | ("\\u" | "\\U") (t_unicode as u) { add_utf8_char lexbuf buf u; read_double_quoted_string buf lexbuf }
  | '\\' [' ' '\t' '\n']* '\n' { newlines lexbuf (Lexing.lexeme lexbuf); read_double_quoted_string buf lexbuf }
  | '\n'      { lexing_error lexbuf "line breaks are not allowed inside strings" }
  | ['\x00'-'\x08' '\x0B'-'\x1F' '\x7F'] as bad_char
    { lexing_error lexbuf @@
        Printf.sprintf "character '%s' is not allowed inside a string literal without escaping"
        (Char.escaped bad_char) 
    }
  | [^ '"' '\\' '\n' '\x00'-'\x08' '\x0B'-'\x1F' '\x7F']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_double_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing double quote" }

and read_single_quoted_string buf =
  parse
  | '''  { validate_unicode lexbuf @@ Buffer.contents buf; STRING (Buffer.contents buf) }
  | '\\' [' ' '\t' '\n']* '\n' { newlines lexbuf (Lexing.lexeme lexbuf); read_single_quoted_string buf lexbuf }
  | '\n' { lexing_error lexbuf "line breaks are not allowed inside strings" }
  | ['\x00'-'\x08' '\x0B'-'\x1F' '\x7F'] as bad_char
    { lexing_error lexbuf @@
        Printf.sprintf "character '%s' is not allowed inside a string literal without escaping"
        (Char.escaped bad_char)
    }
  | [^ ''' '\n' '\x00'-'\x08' '\x0B'-'\x1F' '\x7F']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_single_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing single quote" }

and read_double_quoted_multiline_string buf =
  parse
  | '"' '"' '"' '"' '"'
    {
      Buffer.add_string buf "\"\"";
      validate_unicode lexbuf @@ Buffer.contents buf;
      MULTILINE_STRING (Buffer.contents buf |> trim_left_newline) 
    }
  | '"' '"' '"' '"'
    {
      Buffer.add_string buf "\"";
      validate_unicode lexbuf @@ Buffer.contents buf;
      MULTILINE_STRING (Buffer.contents buf |> trim_left_newline)
    }
  | '"' '"' '"'
    { validate_unicode lexbuf @@ Buffer.contents buf; MULTILINE_STRING (Buffer.contents buf |> trim_left_newline) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_double_quoted_multiline_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_double_quoted_multiline_string buf lexbuf }
  | '\\' [' ' '\t' '\n']* '\n' { newlines lexbuf (Lexing.lexeme lexbuf); read_double_quoted_multiline_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_double_quoted_multiline_string buf lexbuf }
  | ("\\u" | "\\U") (t_unicode as u) { add_utf8_char lexbuf buf u; read_double_quoted_multiline_string buf lexbuf }
  | ['\x00'-'\x08' '\x0B'-'\x1F' '\x7F'] as bad_char
    {
       lexing_error lexbuf @@
         Printf.sprintf "character '%s' is not allowed inside a string literal without escaping"
         (Char.escaped bad_char)
    }
  | '"' [^ '"']
    { Buffer.add_string buf "\""; move_position lexbuf (~-1); read_double_quoted_multiline_string buf lexbuf }
  | '"' '"' [^ '"']
    { Buffer.add_string buf "\"\""; move_position lexbuf (~-1); read_double_quoted_multiline_string buf lexbuf }
  | [^ '"' '\x00'-'\x08' '\n' '\x0B'-'\x1F' '\x7F' '\\']+
    {
      Buffer.add_string buf (Lexing.lexeme lexbuf); 
      read_double_quoted_multiline_string buf lexbuf
    }
  | eof { lexing_error lexbuf "double-quoted multiline string is missing the closing double quotes" }

and read_single_quoted_multiline_string buf =
  parse
  | "'''''"
    {
      Buffer.add_string buf "''";
      validate_unicode lexbuf @@ Buffer.contents buf; 
      MULTILINE_STRING (Buffer.contents buf |> trim_left_newline)
    }
  | "''''"
    {
      Buffer.add_string buf "'";
      validate_unicode lexbuf @@ Buffer.contents buf;
      MULTILINE_STRING (Buffer.contents buf |> trim_left_newline)
    }
  | "'''"   { validate_unicode lexbuf @@ Buffer.contents buf; MULTILINE_STRING (Buffer.contents buf |> trim_left_newline) }
  | '\\' [' ' '\t' '\n']* '\n' { newlines lexbuf (Lexing.lexeme lexbuf); read_single_quoted_multiline_string buf lexbuf }
  | '\n'    { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_single_quoted_multiline_string buf lexbuf }
  | ['\x00'-'\x08' '\x0B'-'\x1F' '\x7F'] as bad_char
    { lexing_error lexbuf @@
        Printf.sprintf "character '%s' is not allowed inside a string literal without escaping"
        (Char.escaped bad_char)
    }
  | (''' [^ '''] | ''' ''' [^ ''']  | [^ ''' '\x00'-'\x08' '\x0B'-'\x1F' '\x7F']+)
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_single_quoted_multiline_string buf lexbuf
    }
  | eof { lexing_error lexbuf "single-quoted multiline string is missing the closing single quotes" }

