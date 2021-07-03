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
}

(** Reusable numeric regexes *)

let t_digit = ['0'-'9']
let t_sign = ['+' '-']
let t_base_prefix = '0' ['x' 'X' 'o' 'O' 'b' 'B']

(* Numbers must not have leading zeroes.
   Digits may be separated by a single underscore.
 *)
let t_integer_part  = '0' | ['1'-'9'] ('_'? t_digit+)*

let t_exponent = ['e' 'E'] t_sign? t_integer_part

(* This covers decimals (42, +42, -42)
   and prefixed base-2/8/16 integers (0xFF, 0o54, 0xDEAD_BEEF...)

   As the spec says:
   "Non-negative integer values may also be expressed in hexadecimal, octal, or binary.
    In these formats, leading + is not allowed and leading zeros are allowed (after the prefix)"
 *)
let t_integer = t_sign? t_integer_part | t_base_prefix '0'* t_integer_part

(* Numbers with an exponent are always interpreted as floats.
   The spec disallows floats with an implicit integer/fractional part, like 42. and .42,
   so we don't cover those cases.
  *)
let t_fractional_part = '.' t_digit ('_'? t_digit+)*
let t_float = t_sign? t_integer_part ((t_fractional_part t_exponent?) | t_exponent)

(* Date and time *)

let t_time_group = t_digit t_digit
let t_local_time = t_time_group ':' t_time_group ':' t_time_group ('.' t_digit+)?


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
| '''
    { read_single_quoted_string (Buffer.create 16) lexbuf }
| '"'
    { read_double_quoted_string (Buffer.create 16) lexbuf }
| '#' [^ '\n']* '\n'
    { Lexing.new_line lexbuf ; token lexbuf }
| eof { EOF }
| _ as bad_char
  { lexing_error lexbuf (Printf.sprintf "unexpected character \'%c\'" bad_char) }

and read_double_quoted_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_double_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_double_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_double_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_double_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_double_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_double_quoted_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_double_quoted_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_double_quoted_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_double_quoted_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_double_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing double quote" }

and read_single_quoted_string buf =
  parse
  | '''       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_single_quoted_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_single_quoted_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_single_quoted_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_single_quoted_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_single_quoted_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_single_quoted_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_single_quoted_string buf lexbuf }
  | '\\' '\'' { Buffer.add_char buf '\''; read_single_quoted_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_single_quoted_string buf lexbuf }
  | '\n'      { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_single_quoted_string buf lexbuf }
  | [^ ''' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_single_quoted_string buf lexbuf
    }
  | eof { lexing_error lexbuf "Quoted string is missing the closing single quote" }
