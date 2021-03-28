include Types

let type_string v =
  match v with
  | TomlString _ -> "string"
  | TomlInteger _ -> "integer"
  | TomlFloat _ -> "float"
  | TomlBoolean _ -> "boolean"
  | TomlLocalTime _ -> "local time"
  | TomlLocalDate _ -> "local date"
  | TomlLocalDateTime _ -> "local date-time"
  | TomlOffsetDateTime _ -> "offset date-time"
  | TomlArray _ -> "array"
  | TomlTable _ | TomlInlineTable _ | TomlTableArray _ -> "table"

exception Key_error of string
exception Type_error of string

let key_error err = raise (Key_error err)
let type_error err = raise (Type_error err)

let list_table_keys t =
  match t with
  | TomlTable os | TomlInlineTable os | TomlTableArray os -> Utils.assoc_keys os
  | _ -> Printf.ksprintf type_error "value must be a table, found %s" (type_string t)

let field ?(default=None) ?(getter=(fun x -> x)) k v =
  match v with
  | TomlTable fs | TomlInlineTable fs | TomlTableArray fs -> begin
      try
        let res = List.assoc k fs in
        getter res
      with
      | Not_found ->
        (match default with
         | None -> Printf.ksprintf key_error "table has no field \"%s\"" k
         | Some default -> default)
    end
  | _ -> Printf.ksprintf type_error "cannot retrieve field \"%s\": value is not a table" k

let string ?(strict=true) v =
  match v with
  | TomlString s -> s
  | _ -> begin
    if strict then Printf.ksprintf type_error "value must be a string, found a %s" (type_string v) else
    match v with
    | TomlInteger i -> string_of_int i
    | TomlFloat f -> Utils.string_of_float f
    | TomlBoolean b -> string_of_bool b
    | _ -> Printf.ksprintf type_error "cannot convert %s to string" (type_string v)
  end

let integer ?(strict=true) v =
  match v with
  | TomlInteger i -> i
  | _ -> begin
    if strict then Printf.ksprintf type_error "value must be an integer, found %s" (type_string v) else
    match v with
     | TomlString s -> int_of_string s
     | TomlBoolean b -> (if b then 1 else 0)
     | _ -> Printf.ksprintf type_error "cannot convert %s to integer" (type_string v)
 end

let bool ?(strict=true) v =
  match v with
  | TomlBoolean b -> b
  | _ -> begin
    if strict then Printf.ksprintf type_error "value must be a boolean, found %s" (type_string v) else
    match v with
    | TomlString s -> (s = "")
    | TomlInteger i -> (i = 0)
    | TomlFloat f -> (f = 0.0)
    | TomlArray a -> (a = [])
    | TomlTable o | TomlInlineTable o | TomlTableArray o ->  (o = [])
    | _ -> false
  end

let table v =
  match v with
  | (TomlTable _ | TomlInlineTable _ | TomlTableArray _) as o -> o
  | _ -> Printf.ksprintf type_error "value must be a table, found %s" (type_string v)

let list ?(strict=true) v =
  match v with
  | TomlArray a -> a
  | _ ->
    if strict then Printf.ksprintf type_error "value must be a list, found %s" (type_string v)
    else [v]

let rec of_json j =
  match j with
  | `Float n -> TomlFloat n
  | `Bool b -> TomlBoolean b
  | `String s -> TomlString s
  | `A js -> TomlArray (List.map of_json js)
  | `O os -> TomlTable (List.map (fun (k, v) -> (k, of_json v)) os)
  | `Null -> TomlTable []

let rec to_json t =
  match t with
  | TomlString s -> `String s
  | TomlInteger i -> `Float (float_of_int i)
  | TomlFloat f -> `Float f
  | TomlBoolean b -> `Bool b
  | TomlLocalTime s -> `String s
  | TomlLocalDate s -> `String s
  | TomlLocalDateTime s -> `String s
  | TomlOffsetDateTime s -> `String s
  | TomlArray xs -> `A (List.map to_json xs)
  | TomlTable os | TomlInlineTable os | TomlTableArray os -> `O (List.map (fun (k, v) -> (k, to_json v)) os)

  
