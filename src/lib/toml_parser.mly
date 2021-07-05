%{
open Types
open Parser_utils

%}

%token EQ
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LDBRACKET
%token RDBRACKET
%token COMMA
%token DOT
%token NEWLINE

(* Primitive values *)
%token <bool> BOOLEAN
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <string> MULTILINE_STRING
%token <string> LOCAL_TIME
%token <string> LOCAL_DATE
%token <string> LOCAL_DATETIME
%token <string> OFFSET_DATETIME
%token <string> KEY

%token EOF

%start <statement list> toml
%%

key:
  | s = KEY
    { s }
  | s = STRING
    { s }

value:
  | b = BOOLEAN
    { TomlBoolean b }
  | i = INTEGER
    { TomlInteger i }
  | f = FLOAT
    { TomlFloat f }
  | s = STRING
    { TomlString s }
  | s = MULTILINE_STRING
    { TomlString s }
  | t = LOCAL_TIME
    { TomlLocalTime t }
  | d = LOCAL_DATE
    { TomlLocalDate d }
  | dt = LOCAL_DATETIME
    { TomlLocalDateTime dt }
  | dt = OFFSET_DATETIME
    { TomlOffsetDateTime dt }
  | a = array
    { TomlArray a }
  | i = inline_table
    { TomlInlineTable i }

(* Arrays allow trailing separators and newlines anywhere inside the square brackets.
   That's why the built-in separated_list() won't do -- we need a custom macro.
 *)
let item_sequence(Sep, X) :=
(*  | (* empty *)
    { [] } *)
  | x = X; 
    { [x] }
  | x = X; Sep; xs = item_sequence(Sep, X);
    { x :: xs }

array:
  | LBRACKET; RBRACKET { [] }
  | LBRACKET; vs = item_sequence(COMMA, value); RBRACKET { vs }

key_value_pair:
  | k = key; EQ; v = value; { (k, v) } 

(* Unlike arrays, inline tables do not allow trailing commas and newlines inside
   (for whatever reason, I hope TOML standard maintainers eventually reconsider it).
   That's why we use an ordinary separated_list() here.
 *)
inline_table:
  | LBRACE; kvs = separated_list(COMMA, key_value_pair); RBRACE { kvs }

(* Non-inline table handling *)

table_path:
  | ks = separated_nonempty_list(DOT, key) { ks }

table_header:
  | LBRACKET; ks = table_path; RBRACKET { ks }

table_array_header:
  | LDBRACKET; ks = table_path; RDBRACKET { ks }

table_entry:
  | kv = key_value_pair; 
    { let (k, v) = kv in Pair (k, v) }
  | ks = table_header
    { TableHeader ks }
  | ks = table_array_header
    { TableArrayHeader ks }

let items_on_lines(X) :=
  | { [] }
  | x = X; 
    { [x] }
  | x = X; NEWLINE+; xs = items_on_lines(X);
    { x :: xs }

table:
  es = items_on_lines(table_entry); { es }

toml: 
  | NEWLINE*; t = table; EOF { t }
