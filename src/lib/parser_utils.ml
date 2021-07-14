type node =
  | NodeString of string
  | NodeInteger of string
  | NodeFloat of string
  | NodeBoolean of string
  | NodeOffsetDateTime of string
  | NodeLocalDateTime of string
  | NodeLocalDate of string
  | NodeLocalTime of string
  | NodeArray of node list
  | NodeTable of (string list * node) list
  | NodeInlineTable of (string list * node) list
  | NodeTableArray of node list
  | Pair of (string list) * node
  | TableHeader of string list
  | TableArrayHeader of string list

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

  
