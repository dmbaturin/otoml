include Types
open Common

let () = Printexc.record_backtrace true

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

let rec update ?(update_table_arrays=false) ?(use_inline_tables=false) value path new_value =
  let make_empty_table use_inline =
    if use_inline then (TomlInlineTable []) else (TomlTable [])
  in
  match path with
  | [] -> failwith "Cannot update a TOML value at an empty path"
  | [p] -> update_field value p new_value
  | p :: ps ->
    let nested_value = field_opt p value |> Option.value ~default:(make_empty_table use_inline_tables) in
    begin match nested_value with
    | TomlTableArray ts ->
      if not update_table_arrays then
        (* Printf.ksprintf type_error "cannot update field %s: value is an array of tables, not a table" p *)
         begin match new_value with
         | Some ((TomlTable _) as v) ->
           update_field value p (Some (TomlTableArray (ts @ [v])))
         | _ ->
           let nested_value = update ~update_table_arrays:update_table_arrays nested_value ps new_value in
           update_field value p (Some nested_value)
         end 
      else
        let body, tail = Utils.split_list ts in
        let tail = Option.value ~default:(TomlTable []) tail in
        let tail = update ~update_table_arrays:update_table_arrays tail ps new_value in
        let ts' = body @ [tail] in
        update_field value p (Some (TomlTableArray ts'))
    | _ ->
      let nested_value = update ~update_table_arrays:update_table_arrays nested_value ps new_value in
      update_field value p (Some nested_value)
    end

module Utils = struct
  include Utils
end

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

  let check_duplicate p' p = 
    match p, p' with
    | TableHeader p, TableHeader p' ->
      if p = p' then Printf.ksprintf (parse_error None) "table [%s] is defined more than once"
        (Utils.string_of_path p)
    | TableHeader p, TableArrayHeader p' ->
      if p = p' then
        let path_str = (Utils.string_of_path p) in
        Printf.ksprintf (parse_error None) "table [%s] is duplicated by an array of tables [[%s]]"
          path_str path_str
    | TableArrayHeader p, TableHeader p' ->
      if p = p' then
        let path_str = (Utils.string_of_path p) in
        Printf.ksprintf (parse_error None) "array of tables [[%s]] is duplicated by a table [%s]"
          path_str path_str
    | _ -> ()

  let check_duplicates xs x = List.iter (check_duplicate x) xs

  (* Takes a child and a parent path and finds the part of the child path unique to the child.
     E.g. `path_complement [1;2;3] [1]` is `[2;3]`.
   *)
  let rec path_complement child parent =
    match child, parent with
    | [], [] ->
      (* They are the same path. *)
      Some []
    | [], (_ :: _) ->
      (* The alleged parent path is longer, so it's not actually a parent path. *)
      None
    | (_ :: _) as ps, [] ->
      (* The parent path is exhausted, so what's left is the part unique to the child. *)
      Some ps
    | (x :: xs), (y :: ys) ->
      if x = y then path_complement xs ys (* Still in the common part of the path. *)
      else None (* Like Buster and Babs Bunny, no relation. *)

  let is_child_path child parent =
    let c = path_complement child parent in
    match c with
    | None | Some [] -> false
    | _ -> true

  let to_pairs ns = List.map (fun (k, v) -> Pair (k, v)) ns

  (* This is for cases that _should not happen_,
     but I haven't proved that they actually _can't_ happen. *)
  let internal_error msg =
    failwith @@ Printf.sprintf "otoml internal error: %s. Please report a bug." msg

  let rec insert ?(if_not_exists=false) ?(append_table_arrays=false) toml path value =
    let check_exists tbl p if_not_exists value =
      let orig_value = field_opt p tbl in
      match orig_value with
      | Some v ->
        if if_not_exists then true
        else duplicate_key_error @@ Printf.sprintf
          "duplicate key \"%s\" overrides a value of type %s with a value of type %s"
          p (type_string v) (type_string value)
      | None -> false
    in
    match path with
    | [] -> internal_error "insert called with empty path"
    | [p] ->
      begin match toml with
      | (TomlTable kvs) as tbl ->
        if append_table_arrays then
          let orig_value = field_opt p toml in
          begin match orig_value, value with
          | Some (TomlTableArray ts), TomlTable _ ->
            let t_array = TomlTableArray (ts @ [value]) in
            update_field tbl p (Some t_array)
          | Some (TomlTableArray _), v ->
           internal_error @@ Printf.sprintf "trying to append a value of type %s to an array of tables"
             (type_string v)
          | Some v, v' ->
            internal_error @@ Printf.sprintf "insert ~append_table_arrays:true called on values of types %s and %s"
              (type_string v) (type_string v')
          | None, _ ->
            internal_error @@ Printf.sprintf "insert ~append_table_arrays:true called on an empty array"
          end
        else if (check_exists tbl p if_not_exists value) then toml
        else TomlTable (kvs @ [p, value])
      | (TomlInlineTable kvs) as tbl ->
        if (check_exists tbl p if_not_exists value) then toml
        else TomlInlineTable (kvs @ [p, value])
     | _ as v ->
        internal_error @@ Printf.sprintf "path is too long (key \"%s\" left, at a value of type %s)"
          p (type_string v)
      end
    | p :: ps ->
      begin match toml with
      | ((TomlTable kvs) | (TomlInlineTable kvs))  as orig_table ->
        let orig_value = field_opt p toml in
        begin match orig_value with
        | Some (((TomlTable _) | (TomlInlineTable _)) as t) ->
          let subtable = insert ~if_not_exists:if_not_exists ~append_table_arrays:append_table_arrays t ps value
          in update_field orig_table p (Some subtable)
        | Some (TomlTableArray ts) ->
          let body, tail = Utils.split_list ts in
          let tail = Option.value ~default:(TomlTable []) tail in
          let tail =
            insert ~if_not_exists:if_not_exists ~append_table_arrays:append_table_arrays tail ps value
          in
          let t_array = TomlTableArray (body @ [tail]) in
          update_field orig_table p (Some t_array)
        | Some (_ as ov) ->
          duplicate_key_error @@ Printf.sprintf
            "duplicate key \"%s\" overrides a value of type %s with a value of type %s"
            p (type_string ov) (type_string value)
        | None ->
          let tbl = TomlTable [] in
          let tbl = insert ~if_not_exists:if_not_exists ~append_table_arrays:append_table_arrays tbl ps value in
          TomlTable (kvs @ [p, tbl])
        end
     | _ as v ->
       duplicate_key_error @@ Printf.sprintf
         "duplicate key \"%s\" overrides a value of type %s with a value of type %s (path remainder: [%s])"
         p (type_string v) (type_string value) (Utils.string_of_path ps)
      end

  let rec value_of_node n =
    match n with
    | NodeInteger n -> TomlInteger (int_of_string n)
    | NodeFloat x -> TomlFloat (float_of_string x)
    | NodeString s -> TomlString s
    | NodeBoolean b -> TomlBoolean (bool_of_string b)
    | NodeOffsetDateTime dt -> TomlOffsetDateTime dt
    | NodeLocalDateTime dt -> TomlLocalDateTime dt
    | NodeLocalDate d -> TomlLocalDate d
    | NodeLocalTime t -> TomlLocalTime t
    | NodeArray ns -> TomlArray (List.map value_of_node ns)
    | NodeInlineTable ns ->
      let ns = to_pairs ns in
      (* Since inline tables cannot contain table arrays,
         the tail returned by from_statements must always be empty.
       *)
      let _, res = from_statements (TomlInlineTable []) [] [] ns in
      res
    | _ -> internal_error "table header or a non-inline table inside a value"
  and from_statements toml parent_path seen_paths statements =
    match statements with
    | [] -> [], toml
    | s :: ss ->
      begin match s with
      | Pair (k, v) ->
        let full_path = parent_path @ k in
        let value = value_of_node v in
        let toml = insert toml full_path value in
        (* Add value paths to seen paths as fake table headers to prevent
           actual table and table array headers from duplicating then. *)
        let seen_paths = (TableHeader full_path) :: seen_paths in 
        from_statements toml parent_path seen_paths ss
      | (TableHeader ks) as n ->
        let () = check_duplicates seen_paths n in
        let seen_paths = n :: seen_paths in
        let toml = insert ~if_not_exists:true toml ks (TomlTable []) in
        from_statements toml ks seen_paths ss
      | (TableArrayHeader ks) as n ->
        let () = check_duplicates seen_paths n in
        let toml = insert ~if_not_exists:true toml ks (TomlTableArray []) in
        if not (is_child_path ks parent_path) then
          let toml = insert ~append_table_arrays:true toml ks (TomlTable []) in
          let stmts, toml = from_statements toml ks [n] ss in
          from_statements toml [] seen_paths stmts
        else
          from_statements toml ks (n :: seen_paths) ss
      | _ -> internal_error "bare value in the AST"
      end

  let parse lexbuf =
    try
      let toml_statements = _parse lexbuf (Toml_parser.Incremental.toml_ast lexbuf.lex_curr_p) in
      let tail_stmts, toml = from_statements (TomlTable []) [] [] toml_statements in
      if tail_stmts <> [] then internal_error "from_statements left a non-empty tail"
      else (Ok toml)
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
