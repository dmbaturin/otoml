include Types
include Common

let force_inline v =
  match v with
  | TomlTable t -> TomlInlineTable t
  | _ as v -> v

type formatter_settings = {
  indent_width: int;
  indent_character: char;
  indent_subtables: bool;
  newline_before_table: bool
}

let make_indent indent settings level =
  if not indent then "" else
  String.make (settings.indent_width * level) settings.indent_character

let rec format_primitive ?(table_path=[]) ?(inline=false) ?(table_array=false) ?(indent=true) ?(indent_level=0) settings callback v =
  match v with
  | TomlString s ->
    callback "\""; callback @@ Utils.escape_string s; callback "\""
  | TomlInteger i ->
    callback @@ string_of_int i
  | TomlFloat f ->
    callback @@ Printf.sprintf "%.2f" f
  | TomlBoolean b ->
    callback @@ string_of_bool b
  | TomlOffsetDateTime dt ->
    callback dt
  | TomlLocalDateTime dt ->
    callback dt
  | TomlLocalDate dt ->
    callback dt
  | TomlLocalTime t ->
    callback t
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
    let () =
      if (table_path <> []) then begin
        if settings.newline_before_table then callback "\n";
        (* Table headers look best when they are at the same indent level as the parent table's keys.
           Since the indent level is incremented by the format_pair function,
           when this function is called on a nested table, the indent level is what it should be
           for the _current table keys_.
           To compensate for this, we decrement the level by one for header printing. *)
        let indent_string = make_indent indent settings (indent_level - 1) in
        if table_array then callback @@ Printf.sprintf "%s[[%s]]\n" indent_string (String.concat "." table_path)
        else callback @@ Printf.sprintf "%s[%s]\n" indent_string (String.concat "." table_path)
      end
    in
    let inline = if table_array then false else inline in
    let t = if table_array then List.map (fun (k, v) -> (k, force_inline v)) t else t in
    let f = format_pair ~table_path:table_path
              ~indent:indent ~indent_level:indent_level ~inline:inline
              ~table_array:table_array
              settings callback
    in
    List.iter f t
  | TomlInlineTable t ->
    let last_index = (List.length t) - 1 in
    callback "{";
    List.iteri (fun n (k, v) ->
      callback @@ Printf.sprintf "%s = " k;
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
and format_pair ?(table_path=[]) ?(indent=true) ?(indent_level=0) ?(inline=false) ?(table_array=false) settings callback (k, v) =
  match v with
  | TomlTable _ as v ->
    let indent_level =
      if settings.indent_subtables then indent_level + 1
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
    callback @@ Printf.sprintf "%s%s = " (make_indent indent settings indent_level) k;
    format_primitive ~table_path:table_path ~indent:indent settings callback v;
    if not inline then callback "\n"

let to_string ?(indent_width=2) ?(indent_character=' ') ?(indent_subtables=false) ?(newline_before_table=true) v =
  let settings = {
    indent_width = indent_width;
    indent_character = indent_character;
    indent_subtables = indent_subtables;
    newline_before_table = newline_before_table
  }
  in
  let buf = Buffer.create 4096 in
  let () = format_primitive settings (Buffer.add_string buf) v in
  Buffer.contents buf

let to_channel ?(indent_width=2) ?(indent_character=' ') ?(indent_subtables=false) ?(newline_before_table=true) chan v =
  let settings = {
    indent_width = indent_width;
    indent_character = indent_character;
    indent_subtables = indent_subtables;
    newline_before_table = newline_before_table
  }
  in
  format_primitive settings (output_string chan) v
