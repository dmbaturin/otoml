include Types
open Common

(* Conversions between different variants of the same type. *)

let table_to_inline t =
  match t with
  | TomlInlineTable _ as t -> t
  | TomlTable t -> TomlInlineTable t
  | _ as t -> Printf.ksprintf type_error "cannot convert %s to an inline table" (type_string t)

let inline_to_table t =
  match t with
  | TomlInlineTable t -> TomlTable t
  | TomlTable _ as t -> t
  | _ as t -> Printf.ksprintf type_error "cannot convert %s to a table" (type_string t)

(* Constructors *)

let string s = TomlString s
let integer n = TomlInteger n
let float n = TomlFloat n
let boolean b = TomlBoolean b
let offset_datetime dt = TomlOffsetDateTime dt
let local_datetime dt = TomlLocalDateTime dt
let local_date d = TomlLocalDate d
let local_time t = TomlLocalTime t
let array xs = TomlArray xs
let table kvs = TomlTable kvs
let inline_table kvs = TomlInlineTable kvs
let table_array xs =
  let is_table t =
    match t with 
    | TomlTable _ | TomlInlineTable _ -> true
    | _ -> false
  in
  if List.for_all is_table xs then TomlTableArray (List.map inline_to_table xs)
  else Printf.ksprintf type_error "cannot create an array of tables: original array contains a non-table item"

(* Accessors *)

let get_table t =
  match t with
  | TomlTable os | TomlInlineTable os -> os
  | _ -> Printf.ksprintf type_error "value is %s, not a table" (type_string t)

let get_string ?(strict=true) t =
  match t with
  | TomlString s -> s
  | _ ->
    begin
      if strict then Printf.ksprintf type_error "value must be a string, found a %s" (type_string t) else
      match t with
      | TomlInteger i -> string_of_int i
      | TomlFloat f -> string_of_float f
      | TomlBoolean b -> string_of_bool b
      | _ -> Printf.ksprintf type_error "cannot convert %s to string" (type_string t)
    end

let get_integer ?(strict=true) t =
  match t with
  | TomlInteger i -> i
  | _ ->
    begin
      if strict then Printf.ksprintf type_error "value must be an integer, found %s" (type_string t) else
      match t with
      | TomlString s -> int_of_string s
      | TomlBoolean b -> (if b then 1 else 0)
      | _ -> Printf.ksprintf type_error "cannot convert %s to integer" (type_string t)
    end

let get_bool ?(strict=true) t =
  match t with
  | TomlBoolean b -> b
  | _ ->
    begin
      if strict then Printf.ksprintf type_error "value must be an boolean, found a %s" (type_string t) else
      match t with
      | TomlString s -> (s = "")
      | TomlInteger i -> (i = 0)
      | TomlFloat f -> (f = 0.0)
      | TomlArray a | TomlTableArray a -> (a = [])
      | TomlTable o | TomlInlineTable o -> (o = [])
      | _ -> false
    end

let get_array ?(strict=true) t =
  match t with
  | TomlArray a | TomlTableArray a -> a
  | _ as v ->
    if strict then Printf.ksprintf type_error "value must be an array, found %s" (type_string t)
    else [v]

(* High-level interfaces *)

let list_table_keys t =
  let t =
    try get_table t
    with Type_error msg -> Printf.ksprintf type_error "cannot list keys: %s" msg
  in
  List.fold_left (fun acc (x, _) -> x :: acc) [] t |> List.rev

let field k t =
  try
    begin
      let t = get_table t in
      let res = List.assoc_opt k t in
      match res with
      | Some res -> res
      | None -> Printf.ksprintf key_error "field \"%s\" not found" k
    end
  with
  | Key_error msg -> Printf.ksprintf key_error "cannot retrieve field \"%s\": %s" k msg
  | Type_error msg -> Printf.ksprintf type_error "cannot retrieve field \"%s\": %s" k msg

let field_opt k t =
  try Some (field k t)
  with Key_error _ -> None

let find accessor value path =
  let make_dotted_path ps = String.concat "." ps in
  let rec aux accessor path value =
    match path with
    | [] -> accessor value
    | p :: ps ->
      let value = field p value in
      aux accessor ps value
  in
  try
    aux accessor path value
  with
  | Key_error msg ->
    Printf.ksprintf key_error "Failed to retrieve a value at %s: %s" (make_dotted_path path) msg
  | Type_error msg ->
    Printf.ksprintf type_error "TOML type error occured while trying to retrieve a value at %s: %s"
      (make_dotted_path path) msg

let find_opt accessor value path =
  try Some (find accessor value path)
  with Key_error _ -> None

let find_or ~default:default value accessor path =
  find_opt accessor value path |> Option.value ~default:default

let update_field value key new_value =
  let rec update assoc key value =
    match assoc with
    | [] ->
      begin
        match value with
        | None -> []
        | Some v -> [(key, v)]
      end
    | (key', value') :: assoc' ->
      if key = key' then
      begin
        match value with
        | None -> assoc'
        | Some v -> (key, v) :: assoc'
      end
      else (key', value') :: (update assoc' key value)
  in
  match value with
  | TomlTable fs -> TomlTable (update fs key new_value)
  | TomlInlineTable fs -> TomlInlineTable (update fs key new_value)
  | _ -> Printf.ksprintf key_error "cannot update field %s: value is %s, not a table" key (type_string value)

let rec update value ?(use_inline_tables=false) path new_value =
  let make_empty_table use_inline =
    if use_inline then (TomlInlineTable []) else (TomlTable [])
  in
  match path with
  | [] -> failwith "Cannot update a TOML value at an empty path"
  | [p] -> update_field value p new_value
  | p :: ps ->
    let nested_value = field_opt p value |> Option.value ~default:(make_empty_table use_inline_tables) in
    let nested_value = update nested_value ps new_value in
    update_field value p (Some nested_value)

module Parser = struct
  open Lexing
  open Parser_utils

  module I = Toml_parser.MenhirInterpreter

  let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (Toml_parser_messages.message (I.number state)) with
        | Not_found -> "invalid syntax (no specific message for this eror)"

  let rec _parse lexbuf (checkpoint : t I.checkpoint) =
    match checkpoint with
    | I.InputNeeded _env ->
      let token = Toml_lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      _parse lexbuf checkpoint
    | I.Shifting _
    | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      _parse lexbuf checkpoint
    | I.HandlingError _env ->
      let line, pos = Parser_utils.get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (Parse_error (Some (line, pos), err))
    | I.Accepted v -> v
    | I.Rejected ->
       raise (Parse_error (None, "invalid syntax (parser rejected the input)"))

  let parse lexbuf =
    try
      let toml = _parse lexbuf (Toml_parser.Incremental.toml lexbuf.lex_curr_p) in
      Ok toml
    with
    | Parse_error (pos, err) ->
      begin match pos with
      | Some (line, pos) ->
          let msg = Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err in
          raise (Syntax_error msg)
      | None -> Error (Printf.sprintf "Syntax error: %s" err)
    end

  let from_channel ic =
    let lexbuf = Lexing.from_channel ic in
    parse lexbuf

  let from_file filename =
    let ic = open_in filename in
    let t = from_channel ic in
    let () = close_in ic in
    t

  let from_string s =
    let lexbuf = Lexing.from_string s in
    parse lexbuf
end
