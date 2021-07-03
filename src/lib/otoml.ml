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

let get_value t = t

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
  let make_dotted_path ps = Utils.string_of_path ps in
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

module Printer = struct
  include Printer
end

module Parser = struct
  open Lexing
  open Parser_utils

  exception Duplicate_key of string

  let duplicate_key_error msg = raise (Duplicate_key msg)
  let parse_error pos msg = raise (Parse_error (pos, msg))

  module I = Toml_parser.MenhirInterpreter

  let get_parse_error env =
    match I.stack env with
    | lazy Nil -> "Invalid syntax"
    | lazy (Cons (I.Element (state, _, _, _), _)) ->
        try (Toml_parser_messages.message (I.number state)) with
        | Not_found -> "invalid syntax (no specific message for this eror)"

  let rec _parse lexbuf (checkpoint (* : (signal list) I.checkpoint *)) =
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

  let rec insert value path new_value =
    match path with
    | [] -> failwith "Cannot update a TOML value at an empty path"
    | [p] -> begin
      match value with
      | TomlTable kvs ->
        (* XXX: Adding to the end makes the operation quadratic,
           but I believe preserving the original order is a worthwhile
           even if the spec does not require it.
           If this becomes a performance issue, it's possible to reverse the tables just once,
           when the structure is complete. *)
        let orig_value = List.assoc_opt p kvs in begin
        match orig_value with
        | Some orig_value ->
          Printf.ksprintf duplicate_key_error "duplicate key \"%s\" (overrides an original value of type %s with %s)"
            p (type_string orig_value) (type_string new_value)
        | None -> TomlTable (kvs @ [(p, new_value)])
        end
      | _ ->
        Printf.ksprintf duplicate_key_error "subtable \"%s\" overrides a previously defined key (original value had type %s)"
          p (type_string new_value)
      end
    | p :: ps ->
      let nested_value = field_opt p value |> Option.value ~default:(TomlTable []) in begin
      match nested_value with
      | TomlTable _ ->
        let nested_value = insert nested_value ps new_value in
        update_field value p (Some nested_value)
      | _ ->
        Printf.ksprintf duplicate_key_error "subtable \"%s\" overrides a previously defined key (original value had type %s)"
          p (type_string value)
      end

  let rec read_table acc stmts =
    match stmts with
    | [] ->
      (* End of file was reached *)
      (List.rev acc, [])
    | stmt :: stmts' ->
      begin match stmt with
      | Pair (k, v) -> read_table ((k, v) :: acc) stmts'
      | _ as s ->
        (* A new table or an array of tables begins here, end of table was reached *)
        (List.rev acc, (s :: stmts'))
      end

  (* The insert function takes care of finding duplicate leaf keys in tables
     and preventing attempts to override a leaf key with a subtable when inserting a value at a path.

     Unfortunately, that is not enough to detect duplicate table headers
     since multiple paths that share the same prefix are perfectly normal,
     only exact duplicates like a "[foo.bar]" written twice are invalid.

     Here we sort the list of table headers in a "natural order"
     and check if any two adjacent items are equal.
     If they are, there's a table declared at least twice in the file.
   *)

  (* The "natural order" is defined as follows:
       0. Empty paths are equal.
       1. Any non-empty path is greater than an empty one.
       2. Non-empty paths with non-equal first items k and k' relate as their first items
          (e.g. [1;2;3] < [2; 3])
       3. Non-empty paths that start with the same item relate as their tails.

     Hopefully this covers all possible cases.
   *)
  let rec compare_paths ks ks' =
    match ks, ks' with
    | [], [] -> 0
    | (_ :: _), [] -> 1
    | [], (_ :: _) -> -1
    | (k :: ks), (k' :: ks') ->
      if k = k' then compare_paths ks ks' else
      compare k k'

  let rec find_duplicates ps =
    match ps with
    | [] | [_] -> None
    | p :: p' :: ps ->
      if p = p' then (Some p) else find_duplicates (p' :: ps)

  let check_duplicate_tables ps =
    let table_headers = List.fold_left (fun acc s -> match s with TableHeader ps -> ps :: acc | _ -> acc) [] ps in
    let duplicates = find_duplicates table_headers in
    match duplicates with
    | None -> ()
    | Some p ->
      Printf.ksprintf (parse_error None) "table [%s] is defined more than once" (Utils.string_of_path p)

  let rec from_statements ?(path=[]) toml ss =
    match ss with
    | [] -> toml
    | s :: ss' -> begin
      match s with
      | Pair (k, v) ->
        let toml = insert toml (path @ [k]) v in begin
          try from_statements ~path:path toml ss'
          with Duplicate_key err ->
            (* A distinct exception is used here to allow it to propagate up
               across multiple recursion levels, so that we can print a full path to the table
               where duplication occurs.
               If we reused Parse_error, it would be caught and re-raised at each leve.
             *)
            parse_error None @@ Printf.sprintf "in table [%s]: %s" (Utils.string_of_path path) err
        end
      | TableHeader ks -> from_statements ~path:ks toml ss'
      | TableArrayHeader ks ->
        let tbl, ss' = read_table [] ss' in
        let existing_value = find_opt get_value toml ks in
        begin match existing_value with
        | Some (TomlTableArray ts) ->
          (* Array of tables already exists, we need to append a new table to it. *)
          let toml = update toml ks (Some (TomlTableArray (ts @ [TomlTable tbl]))) in
          from_statements ~path:path toml ss'
        | None ->
          (* It didn't exist before, we need to create it now. *)
          let toml = insert toml ks (TomlTableArray [TomlTable tbl]) in
          from_statements ~path:path toml ss'
        | Some (_ as v) ->
          (* Some other value already exists at that path, so it's not a valid TOML. *)
          parse_error None @@ Printf.sprintf "cannot create a table array [[%s]], it would override a previously defined value of type %s"
            (Utils.string_of_path ks) (type_string v)
        end
    end

  let parse lexbuf =
    try
      let toml_statements = _parse lexbuf (Toml_parser.Incremental.toml lexbuf.lex_curr_p) in
      let () = check_duplicate_tables toml_statements in
      let toml = from_statements (TomlTable []) toml_statements in
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
