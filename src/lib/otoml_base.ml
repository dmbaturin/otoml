include Common

module Utils = struct
  include Utils
end

include Impl_sigs

(* Internal exception for tracking non-existent fields.
   Since it's possible to query a full path like "table.subtable.field",
   error messages should tell the user which exact component of that path
   does not exist.
   So field lookup functions will raise this exception to signal bad field,
   and the public interface will convert it to a complete Key_error. *)
exception Field_error of string

(* Raised by lookup functions when the TOML value given to them
   is not a table at all and can't have fields;
   to differentiate from a situation when the target value exists,
   but it's type is wrong for the accessor function passed by the user. *)
exception Not_a_table of string

module Make (N: TomlNumber) (D: TomlDate) = struct
  include Common

  type toml_integer = N.int
  type toml_float = N.float
  type toml_date = D.t

  type t =
    | TomlString of string
    | TomlInteger of toml_integer
    | TomlFloat of toml_float
    | TomlBoolean of bool
    | TomlOffsetDateTime of toml_date
    | TomlLocalDateTime of toml_date
    | TomlLocalDate of toml_date
    | TomlLocalTime of toml_date
    | TomlArray of t list
    | TomlTable of (string * t) list
    | TomlInlineTable of (string * t) list
    | TomlTableArray of t list

  let type_string v =
    match v with
    | TomlString _         -> "string"
    | TomlInteger _        -> "integer"
    | TomlFloat _          -> "float"
    | TomlBoolean _        -> "boolean"
    | TomlLocalTime _      -> "local time"
    | TomlLocalDate _      -> "local date"
    | TomlLocalDateTime _  -> "local date-time"
    | TomlOffsetDateTime _ -> "offset date-time"
    (* For the purpose of type error printing,
       we don't make a difference between syntactic variants
       because there's no cases when that would matter. *)
    | TomlArray _          -> "array"
    | TomlTableArray _     -> "array"
    | TomlTable _          -> "table"
    | TomlInlineTable _    -> "table"


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

  let get_table_values accessor t =
    let kvs = get_table t in
    List.map (fun (k, v) -> (k, accessor v)) kvs

  let get_string ?(strict=true) t =
    match t with
    | TomlString s -> s
    | _ ->
      begin
	if strict then Printf.ksprintf type_error "value must be a string, found %s" (type_string t) else
	match t with
	| TomlInteger i -> N.int_to_string i
	| TomlFloat f -> N.float_to_string f
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
        | TomlFloat f -> N.int_of_float f
	| TomlString s ->
          begin
            try N.int_of_string s
            with Failure _ ->
              Printf.ksprintf type_error "string \"%s\" does not represent a valid integer"
              (Utils.escape_string s)
          end
	| TomlBoolean b -> N.int_of_boolean b
	| _ -> Printf.ksprintf type_error "cannot convert %s to integer" (type_string t)
      end

  let get_float ?(strict=true) t =
    match t with
    | TomlInteger i -> N.float_of_int i
    | TomlFloat f -> f
    | _ ->
      begin
        if strict then Printf.ksprintf type_error "value must be a float, found %s" (type_string t) else
        match t with
        | TomlString s ->
          begin
            try N.float_of_string s
            with Failure _ -> Printf.ksprintf type_error
              "string \"%s\" does not represent a valid floating point number"
              (Utils.escape_string s)
          end
        | TomlBoolean b -> N.float_of_boolean b
        | _ -> Printf.ksprintf type_error "cannot convert %s to float" (type_string t)
      end

  let get_boolean ?(strict=true) t =
    match t with
    | TomlBoolean b -> b
    | _ ->
      begin
	if strict then Printf.ksprintf type_error "value must be a boolean, found %s" (type_string t) else
	match t with
	| TomlString s -> (s = "")
	| TomlInteger i -> N.int_to_boolean i
	| TomlFloat f -> N.float_to_boolean f
	| TomlArray a | TomlTableArray a -> (a <> [])
	| TomlTable o | TomlInlineTable o -> (o <> [])
	| _ -> false
      end

  let get_array ?(strict=true) accessor t =
    match t with
    | TomlArray a | TomlTableArray a -> List.map accessor a
    | _ as v ->
      if strict then Printf.ksprintf type_error "value must be an array, found %s" (type_string t)
      else List.map accessor [v]

  let get_value t = t

  let get_offset_datetime ?(strict=true) t =
    match t with
    | TomlOffsetDateTime dt -> dt
    | _ as v->
      begin
        if strict then Printf.ksprintf type_error "value must be an offset datetime, found %s" (type_string t) else
        match v with
        | TomlString s ->
          (try D.offset_datetime_of_string s
           with Failure msg -> Printf.ksprintf type_error "failed to convert %s to offset datetime: %s" s msg)
        | _ -> Printf.ksprintf type_error "cannot convert %s to offset datetime" (type_string v)
      end

  let get_local_datetime ?(strict=true) t =
    match t with
    | TomlLocalDateTime dt -> dt
    | _ as v ->
      begin
        if strict then Printf.ksprintf type_error "value must be a local datetime, found %s" (type_string t) else
        match v with
        | TomlString s ->
          (try D.local_datetime_of_string s
           with Failure msg -> Printf.ksprintf type_error "failed to convert %s to local datetime: %s" s msg)
        | _ -> Printf.ksprintf type_error "cannot convert %s to local datetime" (type_string v)
      end

  let get_local_date ?(strict=true) t =
    match t with
    | TomlLocalDate dt -> dt
    | _ as v ->
      begin
        if strict then Printf.ksprintf type_error "value must be a local date, found %s" (type_string t) else
        match v with
        | TomlString s ->
          (try D.local_date_of_string s
           with Failure msg -> Printf.ksprintf type_error "failed to convert %s to local date: %s" s msg)
        | _ -> Printf.ksprintf type_error "cannot convert %s to local date" (type_string v)
      end

  let get_local_time ?(strict=true) t =
    match t with
    | TomlLocalTime dt -> dt
    | _ as v -> 
      begin
        if strict then Printf.ksprintf type_error "value must be a local time, found %s" (type_string t) else
        match v with
        | TomlString s ->
          (try D.local_time_of_string s
           with Failure msg -> Printf.ksprintf type_error "failed to convert %s to local time: %s" s msg)
        | _ -> Printf.ksprintf type_error "cannot convert %s to local time" (type_string v)
      end

  (* Datetime retrieval convenience functions *)

  let get_datetime t =
    match t with
    | TomlOffsetDateTime dt -> dt
    | TomlLocalDateTime dt -> dt
    | _ ->
        Printf.ksprintf type_error "value must be a datetime (local or offset), found %s" (type_string t)

  let get_date t =
    match t with
    | TomlOffsetDateTime dt -> dt
    | TomlLocalDateTime dt -> dt
    | TomlLocalDate dt -> dt
    | _ -> Printf.ksprintf type_error "value must be a date or datetime, found %s" (type_string t)

  (* Combinators *)

  let get_opt f t =
    try Some (f t)
    with Type_error _ -> None

  let get_result f t =
    try Ok (f t)
    with Type_error msg -> Error msg

  (* High-level interfaces *)

  let list_table_keys t =
    let t =
      try get_table t
      with Type_error msg -> Printf.ksprintf type_error "cannot list table keys: %s" msg
    in
    List.fold_left (fun acc (x, _) -> x :: acc) [] t |> List.rev

  let list_table_keys_exn = list_table_keys

  let list_table_keys_result t =
    try Ok (list_table_keys_exn t)
    with Type_error msg -> Error msg

  let _field k t =
    begin
      let t = get_table t in
      let res = List.assoc_opt k t in
      match res with
      | Some res -> res
      | None -> raise (Field_error k)
    end

  let field k t =
    try _field k t
    with Field_error msg -> Printf.ksprintf key_error "field \"%s\" not found"
      (Utils.make_printable_key k) msg

  let field_opt k t =
    try Some (field k t)
    with Key_error _ -> None

  let find value accessor path =
    let make_dotted_path ps = Utils.string_of_path ps in
    let check_if_table k v =
      match v with
      | TomlTable _ | TomlInlineTable _ -> ()
      | _ as v ->
        Printf.ksprintf (fun s -> raise (Not_a_table s)) "value at field \"%s\" is %s, not a table"
          (Utils.make_printable_key k) (type_string v)
    in
    let rec aux accessor path value =
      match path with
      | [] -> accessor value
      | p :: ps ->
        let _ = check_if_table p value in
	let value = _field p value in
	aux accessor ps value
    in
    try
      aux accessor path value
    with
    | Field_error msg ->
      (* The Field_error will contain the first non-existent field in the path.
         E.g. trying to find [table.subtable.no_such_field] should produce
         "Failed to retrieve ... "table.subtable.bad_field": field "bad_field" not found" *)
      Printf.ksprintf key_error "Failed to retrieve a value at %s: field %s not found"
        (make_dotted_path path) (Utils.make_printable_key msg)
    | Not_a_table k ->
      (* Something in the middle of the path isn't a table,
         or path is too long. *)
      Printf.ksprintf key_error "Failed to retrieve a value at %s: %s" (make_dotted_path path) k
    | Type_error msg ->
      Printf.ksprintf type_error "Unexpected TOML value type at key %s: %s"
	(make_dotted_path path) msg

  let find_exn = find

  let find_opt value accessor path =
    try Some (find value accessor path)
    with Key_error _ -> None

  let find_or ~default:default value accessor path =
    find_opt value accessor path |> Option.value ~default:default

  let find_result value accessor path =
    try Ok (find value accessor path)
    with
    | Key_error msg  -> Error msg
    | Type_error msg -> Error msg

  let path_exists value path =
    let res = find_opt value get_value path in
    match res with
    | Some _ -> true
    | None   -> false

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
    | _ -> Printf.ksprintf key_error "cannot update field %s: value is %s, not a table"
      (Utils.make_printable_key key) (type_string value)

  let rec update ?(use_inline_tables=false) value path new_value =
    let make_empty_table use_inline =
      if use_inline then (TomlInlineTable []) else (TomlTable [])
    in
    match path with
    | [] -> Printf.ksprintf key_error "Cannot update a TOML value at an empty path"
    | [p] -> update_field value p new_value
    | p :: ps ->
      let nested_value = field_opt p value |> Option.value ~default:(make_empty_table use_inline_tables) in
      let nested_value = update nested_value ps new_value in
      update_field value p (Some nested_value)

  let update_result ?(use_inline_tables=false) value path new_value =
    try Ok (update ~use_inline_tables:use_inline_tables value path new_value)
    with Key_error msg | Type_error msg -> Error msg

  let string_of_path  = Utils.string_of_path

  module Printer = struct
    let force_inline v =
      match v with
      | TomlTable t -> TomlInlineTable t
      | _ as v -> v

    type formatter_settings = {
      indent_width: int;
      indent_character: char;
      indent_subtables: bool;
      newline_before_table: bool;
      collapse_tables: bool
    }

    let make_indent indent settings level =
      if not indent then "" else
      String.make (settings.indent_width * level) settings.indent_character

    let has_nontable_items t =
      (* Headers of empty tables _must_ be displayed
         (since "table exists and has no items" and "table does not exist" are different conditions),
         so for the purpose of collapsing tables to improve readability,
         an empty table is _not_ collapsible.
       *)
      if t = [] then true else
      List.fold_left (fun acc (_, v) -> (match v with TomlTable _ -> false | _ -> true) || acc) false t

    let reorder_items t =
      let rec aux acc_items acc_tbls vs =
        match vs with
        | [] -> (List.rev acc_items) @ (List.rev acc_tbls)
        | (k, v) :: vs' ->
          begin match v with
          | TomlTable _ -> aux acc_items ((k, v) :: acc_tbls) vs'
          | _ -> aux ((k, v) :: acc_items) acc_tbls vs'
          end
       in aux [] [] t

    let is_table_array t =
      if t = [] then false else
      List.fold_left (fun acc v -> (match v with TomlTable _ | TomlInlineTable _ -> true | _ -> false) && acc) true t

    let rec _force_table_arrays t =
      match t with
      | TomlArray vs ->
        if (is_table_array vs) then
          let vs = List.map _force_table_arrays vs in
          TomlTableArray (List.map inline_to_table vs)
        else TomlArray vs
      | TomlTable kvs ->
        TomlTable (List.map (fun (k, v) -> (k, _force_table_arrays v)) kvs)
      | _ -> t

    let rec format_primitive ?(table_path=[]) ?(inline=false) ?(table_array=false) ?(indent=true) ?(indent_level=0) settings callback v =
      match v with
      | TomlString s ->
          (* Use multi-line string syntax for strings with line breaks. *)
          if String.contains s '\n' then
            begin
              (* As the spec says:
                 >A newline immediately following the opening delimiter will be trimmed.

                 Thus it's safe to add a line break after the opening quotes,
                 which I think is much more readable.
               *)
              callback "\"\"\"\n";
              callback @@ Utils.escape_string ~exclude:['\r'; '\n'] s;
              callback "\"\"\"";
            end
          else
            begin
              callback "\"";
              callback @@ Utils.escape_string s;
              callback "\""
            end
      | TomlInteger i ->
	callback @@ N.int_to_string i
      | TomlFloat f ->
	callback @@ N.float_to_string f
      | TomlBoolean b ->
	callback @@ string_of_bool b
      | TomlOffsetDateTime dt ->
	callback @@ D.offset_datetime_to_string dt
      | TomlLocalDateTime dt ->
	callback @@ D.local_datetime_to_string dt
      | TomlLocalDate dt ->
	callback @@ D.local_date_to_string dt
      | TomlLocalTime t ->
	callback @@ D.local_time_to_string t
      | TomlArray a ->
	let a = List.map force_inline a in
	let last_index = (List.length a) - 1 in
	callback "[";
	List.iteri (fun n v ->
	  (* Nothing inside an array should be indented. *)
	  format_primitive ~indent:false settings callback v;
	  (* Avoid trailing commas after the last item (even though the 1.0 spec allows them). *)
	  if n <> last_index then callback ", ")
	a;
	callback "]"
      | TomlTable t ->
        let t = reorder_items t in
        let is_shell_table = has_nontable_items t in
	let () =
	  if (table_path <> []) && (not settings.collapse_tables || is_shell_table) then begin
	    if settings.newline_before_table then callback "\n";
	    (* Table headers look best when they are at the same indent level as the parent table's keys.
	       Since the indent level is incremented by the format_pair function,
	       when this function is called on a nested table, the indent level is what it should be
	       for the _current table keys_.
	       To compensate for this, we decrement the level by one for header printing. *)
	    let indent_string = make_indent indent settings (indent_level - 1) in
	    let path_string = Utils.string_of_path table_path in
	    if table_array then callback @@ Printf.sprintf "%s[[%s]]\n" indent_string path_string
	    else callback @@ Printf.sprintf "%s[%s]\n" indent_string path_string
	  end
	in
	let inline = if table_array then false else inline in
	let t = if table_array then List.map (fun (k, v) -> (k, force_inline v)) t else t in
	let f = format_pair ~table_path:table_path
		  ~indent:indent ~indent_level:indent_level
                  ~inline:inline ~table_array:table_array 
		  settings callback
	in
	List.iter f t
      | TomlInlineTable t ->
	let last_index = (List.length t) - 1 in
	callback "{";
	List.iteri (fun n (k, v) ->
	  callback @@ Printf.sprintf "%s = " (Utils.make_printable_key k);
	  (* If an _inline_ table contains other tables or table arrays,
	     we have to force them all to inline table format to produce valid TOML. *)
	  let v = force_inline v in
	  (* We also need to disable key indentation, else it will look weird. *)
	  format_primitive ~table_path:[] ~indent:false settings callback v;
	  if n <> last_index then callback ", ")
	t;
	callback "}"
      | TomlTableArray _ ->
	(* A non-inline table array must have a [[$name]] header, but $name has to come from somewhere,
	   so, unlike other values, it's impossible to render it in isolation.
	   Only the render_pair function called from a table can render table arrays correctly. *)
	failwith "TOML arrays of tables cannot be formatted out of the parent table context"
    and format_pair ?(table_path=[]) ?(indent=true) ?(indent_level=0)
        ?(inline=false) ?(table_array=false) settings callback (k, v) =
      match v with
      | TomlTable kvs as v ->
        let no_level_increase = (has_nontable_items kvs) && settings.collapse_tables in
	let indent_level =
	  if settings.indent_subtables && not no_level_increase then indent_level + 1
	  else if indent_level < 1 then indent_level + 1 else indent_level
	in
	format_primitive ~table_path:(table_path @ [k]) ~indent_level:indent_level ~table_array:table_array
	  settings callback v
      | TomlTableArray v ->
	let v = List.map (fun v -> (k, v)) v in
	let f = format_pair ~table_path:table_path ~indent:indent ~indent_level:indent_level
		  ~inline:inline ~table_array:true
		  settings callback
	in
	List.iter f v
      | _ as v ->
	let k = Utils.make_printable_key k in
	callback @@ Printf.sprintf "%s%s = " (make_indent indent settings indent_level) k;
	format_primitive ~table_path:table_path ~indent:indent settings callback v;
	if not inline then callback "\n"

    let to_string ?(indent_width=2) ?(indent_character=' ') ?(indent_subtables=false)
                  ?(newline_before_table=true) ?(collapse_tables=false) ?(force_table_arrays=false) v =
      let settings = {
	indent_width = indent_width;
	indent_character = indent_character;
	indent_subtables = indent_subtables;
	newline_before_table = newline_before_table;
        collapse_tables= collapse_tables
      }
      in
      let buf = Buffer.create 4096 in
      let v = if force_table_arrays then _force_table_arrays v else v in
      let () = format_primitive settings (Buffer.add_string buf) v in
      Buffer.contents buf

    let to_channel ?(indent_width=2) ?(indent_character=' ') ?(indent_subtables=false)
                   ?(newline_before_table=true) ?(collapse_tables=false) ?(force_table_arrays=false) chan v =
      let settings = {
	indent_width = indent_width;
	indent_character = indent_character;
	indent_subtables = indent_subtables;
	newline_before_table = newline_before_table;
        collapse_tables= collapse_tables
      }
      in
      let v = if force_table_arrays then _force_table_arrays v else v in
      format_primitive settings (output_string chan) v
  end

  module Parser = struct
    open Lexing
    open Parser_utils

    let parse_error pos msg = raise (Parse_error (pos, msg))

    module MI = Toml_parser.MenhirInterpreter

    let get_parse_error env =
      match MI.stack env with
      | lazy Nil -> "Invalid syntax"
      | lazy (Cons (MI.Element (state, _, _, _), _)) ->
	  try (String.trim (Toml_parser_messages.message (MI.number state))) with
	  | Not_found -> "invalid syntax (no specific message for this error)"

    let rec _parse state lexbuf (checkpoint : (node list) MI.checkpoint ) =
      match checkpoint with
      | MI.InputNeeded _env ->
	let state, token = Toml_lexer.token state lexbuf in
	let startp = lexbuf.lex_start_p
	and endp = lexbuf.lex_curr_p in
	let checkpoint = MI.offer checkpoint (token, startp, endp) in
	_parse state lexbuf checkpoint
      | MI.Shifting _
      | MI.AboutToReduce _ ->
	let checkpoint = MI.resume checkpoint in
	_parse state lexbuf checkpoint
      | MI.HandlingError _env ->
	let line, pos = Parser_utils.get_lexing_position lexbuf in
	let err = get_parse_error _env in
	raise (Parse_error (Some (line, pos), err))
      | MI.Accepted v -> v
      | MI.Rejected ->
	 raise (Parse_error (None, "invalid syntax (parser rejected the input)"))

    let check_duplicate p' p = 
      match p, p' with
      | TableHeader p, TableHeader p' ->
	if p = p' then Printf.ksprintf duplicate_key_error "table [%s] is defined more than once"
	  (Utils.string_of_path p)
      | TableHeader p, TableArrayHeader p' ->
	if p = p' then
	  let path_str = (Utils.string_of_path p) in
	  Printf.ksprintf duplicate_key_error "table [%s] is duplicated by an array of tables [[%s]]"
	    path_str path_str
      | TableArrayHeader p, TableHeader p' ->
	if p = p' then
	  let path_str = (Utils.string_of_path p) in
	  Printf.ksprintf duplicate_key_error "array of tables [[%s]] is duplicated by a table [%s]"
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
      | NodeInteger n -> TomlInteger (N.int_of_string n)
      | NodeFloat x -> TomlFloat (N.float_of_string x)
      | NodeString s -> TomlString s
      | NodeBoolean b -> TomlBoolean (bool_of_string b)
      | NodeOffsetDateTime dt -> TomlOffsetDateTime (D.offset_datetime_of_string dt)
      | NodeLocalDateTime dt -> TomlLocalDateTime (D.local_datetime_of_string dt)
      | NodeLocalDate d -> TomlLocalDate (D.local_date_of_string d)
      | NodeLocalTime t -> TomlLocalTime (D.local_time_of_string t)
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

    let format_parse_error pos err =
      match pos with
      | Some (line, pos) ->
        Printf.sprintf "Syntax error on line %d, character %d: %s" line pos err
      | None ->
        Printf.sprintf "Parse error: %s" err
      

    let parse lexbuf =
      (* Make a fresh lexer context *)
      let state = [] in
      let toml_statements = _parse state lexbuf (Toml_parser.Incremental.toml_ast lexbuf.lex_curr_p) in
      let tail_stmts, toml = from_statements (TomlTable []) [] [] toml_statements in
      if tail_stmts <> [] then internal_error "from_statements left a non-empty tail"
      else toml

    let from_channel ic =
      let lexbuf = Lexing.from_channel ic in
      parse lexbuf

    let from_file filename =
      let ic = open_in filename in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () -> from_channel ic)

    let from_string s =
      let lexbuf = Lexing.from_string s in
      parse lexbuf

    let from_string_result s =
      try Ok (from_string s)
      with
      | Parse_error (pos, err) -> Error (format_parse_error pos err)
      | Duplicate_key err -> Error err
      | Failure err -> Error (Printf.sprintf "otoml internal error: %s" err)

    let from_channel_result ic =
      try Ok (from_channel ic)
      with
      | Parse_error (pos, err) -> Error (format_parse_error pos err)
      | Sys_error err -> Error err
      | Failure err -> Error (Printf.sprintf "otoml internal error: %s" err)

    let from_file_result f =
      try Ok (from_file f)
      with
      | Parse_error (pos, err) -> Error (format_parse_error pos err)
      | Sys_error err -> Error err
      | Failure err -> Error (Printf.sprintf "otoml internal error: %s" err)
  end

  module Helpers = struct
    let find_string ?(strict=true) toml path = find toml (get_string ~strict:strict) path
    let find_string_exn ?(strict=true) toml path = find toml (get_string ~strict:strict) path
    let find_string_opt ?(strict=true) toml path = find_opt toml (get_string ~strict:strict) path
    let find_string_result ?(strict=true) toml path = find_result toml (get_string ~strict:strict ) path
    let find_string_default ?(strict=true) ~default toml path =
      try find toml (get_string ~strict:strict) path
      with Key_error _ -> default

    let find_strings ?(strict=true) toml path = find toml (get_array ~strict:false (get_string ~strict:strict)) path
    let find_strings_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_string ~strict:strict)) path
    let find_strings_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_string ~strict:strict)) path
    let find_strings_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_string ~strict:strict)) path
    let find_strings_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_string ~strict:strict)) path
      with Key_error _ -> default

    let find_integer ?(strict=true) toml path = find toml (get_integer ~strict:strict) path
    let find_integer_exn ?(strict=true) toml path = find toml (get_integer ~strict:strict) path
    let find_integer_opt ?(strict=true) toml path = find_opt toml (get_integer ~strict:strict) path
    let find_integer_result ?(strict=true) toml path = find_result toml (get_integer ~strict:strict ) path
    let find_integer_default ?(strict=true) ~default toml path =
      try find toml (get_integer ~strict:strict) path
      with Key_error _ -> default

    let find_integers ?(strict=true) toml path = find toml (get_array ~strict:false (get_integer ~strict:strict)) path
    let find_integers_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_integer ~strict:strict)) path
    let find_integers_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_integer ~strict:strict)) path
    let find_integers_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_integer ~strict:strict)) path
    let find_integers_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_integer ~strict:strict)) path
      with Key_error _ -> default

    let find_float ?(strict=true) toml path = find toml (get_float ~strict:strict) path
    let find_float_exn ?(strict=true) toml path = find toml (get_float ~strict:strict) path
    let find_float_opt ?(strict=true) toml path = find_opt toml (get_float ~strict:strict) path
    let find_float_result ?(strict=true) toml path = find_result toml (get_float ~strict:strict ) path
    let find_float_default ?(strict=true) ~default toml path =
      try find toml (get_float ~strict:strict) path
      with Key_error _ -> default

    let find_floats ?(strict=true) toml path = find toml (get_array ~strict:false (get_float ~strict:strict)) path
    let find_floats_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_float ~strict:strict)) path
    let find_floats_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_float ~strict:strict)) path
    let find_floats_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_float ~strict:strict)) path
    let find_floats_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_float ~strict:strict)) path
      with Key_error _ -> default

    let find_boolean ?(strict=true) toml path = find toml (get_boolean ~strict:strict) path
    let find_boolean_exn ?(strict=true) toml path = find toml (get_boolean ~strict:strict) path
    let find_boolean_opt ?(strict=true) toml path = find_opt toml (get_boolean ~strict:strict) path
    let find_boolean_result ?(strict=true) toml path = find_result toml (get_boolean ~strict:strict ) path
    let find_boolean_default ?(strict=true) ~default toml path =
      try find toml (get_boolean ~strict:strict) path
      with Key_error _ -> default

    let find_booleans ?(strict=true) toml path = find toml (get_array ~strict:false (get_boolean ~strict:strict)) path
    let find_booleans_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_boolean ~strict:strict)) path
    let find_booleans_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_boolean ~strict:strict)) path
    let find_booleans_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_boolean ~strict:strict)) path
    let find_booleans_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_boolean ~strict:strict)) path
      with Key_error _ -> default

    let find_offset_datetime ?(strict=true) toml path = find toml (get_offset_datetime ~strict:strict) path
    let find_offset_datetime_exn ?(strict=true) toml path = find toml (get_offset_datetime ~strict:strict) path
    let find_offset_datetime_opt ?(strict=true) toml path = find_opt toml (get_offset_datetime ~strict:strict) path
    let find_offset_datetime_result ?(strict=true) toml path = find_result toml (get_offset_datetime ~strict:strict ) path
    let find_offset_datetime_default ?(strict=true) ~default toml path =
      try find toml (get_offset_datetime ~strict:strict) path
      with Key_error _ -> default

    let find_offset_datetimes ?(strict=true) toml path = find toml (get_array ~strict:false (get_offset_datetime ~strict:strict)) path
    let find_offset_datetimes_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_offset_datetime ~strict:strict)) path
    let find_offset_datetimes_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_offset_datetime ~strict:strict)) path
    let find_offset_datetimes_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_offset_datetime ~strict:strict)) path
    let find_offset_datetimes_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_offset_datetime ~strict:strict)) path
      with Key_error _ -> default

    let find_local_datetime ?(strict=true) toml path = find toml (get_local_datetime ~strict:strict) path
    let find_local_datetime_exn ?(strict=true) toml path = find toml (get_local_datetime ~strict:strict) path
    let find_local_datetime_opt ?(strict=true) toml path = find_opt toml (get_local_datetime ~strict:strict) path
    let find_local_datetime_result ?(strict=true) toml path = find_result toml (get_local_datetime ~strict:strict ) path
    let find_local_datetime_default ?(strict=true) ~default toml path =
      try find toml (get_local_datetime ~strict:strict) path
      with Key_error _ -> default

    let find_local_datetimes ?(strict=true) toml path = find toml (get_array ~strict:false (get_local_datetime ~strict:strict)) path
    let find_local_datetimes_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_local_datetime ~strict:strict)) path
    let find_local_datetimes_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_local_datetime ~strict:strict)) path
    let find_local_datetimes_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_local_datetime ~strict:strict)) path
    let find_local_datetimes_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_local_datetime ~strict:strict)) path
      with Key_error _ -> default

    let find_local_date ?(strict=true) toml path = find toml (get_local_date ~strict:strict) path
    let find_local_date_exn ?(strict=true) toml path = find toml (get_local_date ~strict:strict) path
    let find_local_date_opt ?(strict=true) toml path = find_opt toml (get_local_date ~strict:strict) path
    let find_local_date_result ?(strict=true) toml path = find_result toml (get_local_date ~strict:strict ) path
    let find_local_date_default ?(strict=true) ~default toml path =
      try find toml (get_local_date ~strict:strict) path
      with Key_error _ -> default

    let find_local_dates ?(strict=true) toml path = find toml (get_array ~strict:false (get_local_date ~strict:strict)) path
    let find_local_dates_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_local_date ~strict:strict)) path
    let find_local_dates_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_local_date ~strict:strict)) path
    let find_local_dates_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_local_date ~strict:strict)) path
    let find_local_dates_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_local_date ~strict:strict)) path
      with Key_error _ -> default

    let find_local_time ?(strict=true) toml path = find toml (get_local_time ~strict:strict) path
    let find_local_time_exn ?(strict=true) toml path = find toml (get_local_time ~strict:strict) path
    let find_local_time_opt ?(strict=true) toml path = find_opt toml (get_local_time ~strict:strict) path
    let find_local_time_result ?(strict=true) toml path = find_result toml (get_local_time ~strict:strict ) path
    let find_local_time_default ?(strict=true) ~default toml path =
      try find toml (get_local_time ~strict:strict) path
      with Key_error _ -> default

    let find_local_times ?(strict=true) toml path = find toml (get_array ~strict:false (get_local_time ~strict:strict)) path
    let find_local_times_exn ?(strict=true) toml path = find toml (get_array ~strict:false (get_local_time ~strict:strict)) path
    let find_local_times_opt ?(strict=true) toml path = find_opt toml (get_array ~strict:false (get_local_time ~strict:strict)) path
    let find_local_times_result ?(strict=true) toml path = find_result toml (get_array ~strict:false (get_local_time ~strict:strict)) path
    let find_local_times_default ?(strict=true) ~default toml path =
      try find toml (get_array ~strict:false (get_local_time ~strict:strict)) path
      with Key_error _ -> default

  end
end
