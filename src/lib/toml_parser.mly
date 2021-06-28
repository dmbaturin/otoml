%{
open Types

%}

%token EQ
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT

(* Primitive values *)
%token <bool> BOOLEAN
%token <int> INTEGER
%token <float> FLOAT
%token <string> STRING
%token <string> KEY

%token EOF

%start <t> toml
%%

toml: v = value { v }

value:
  | b = BOOLEAN { TomlBoolean(b) }
  | i = INTEGER { TomlInteger(i) }
  | f = FLOAT { TomlFloat(f) }
  | s = STRING { TomlString(s) }
  | a = array { TomlArray(a) }

array:
  LBRACKET; vs = separated_list(COMMA, value); option(COMMA); RBRACKET { vs }
