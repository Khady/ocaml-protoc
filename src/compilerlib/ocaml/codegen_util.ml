module T = Ocaml_types

let sp x =  Printf.sprintf x 
(** [sp x] same as sprintf but prefixed with new line *)

let let_decl_of_and = function | Some _ -> "and" | None -> "let rec" 

let string_of_basic_type = function 
  | T.String -> "string"
  | T.Float  -> "float"
  | T.Int    -> "int"
  | T.Int32  -> "int32"
  | T.Int64  -> "int64"
  | T.Bytes  -> "bytes"
  | T.Bool   -> "bool"

let string_of_user_defined = function 
  | {T.module_ = None; T.type_name} -> type_name
  | {T.module_ = Some module_; T.type_name} -> module_ ^ "." ^ type_name

let string_of_field_type = function 
  | T.Unit -> "unit"
  | T.Basic_type bt -> string_of_basic_type bt 
  | T.User_defined_type ud -> string_of_user_defined ud  

let string_of_repeated_type = function
  | T.Rt_list -> "list"
  | T.Rt_repeated_field -> "Pbrt.Repeated_field.t"

let string_of_associative_type = function
  | T.Al_list -> "list"

let string_of_record_field_type = function
  | T.Required (field_type, _, _, _) -> 
      string_of_field_type field_type
  | T.Optional (field_type, _, _, _) -> 
      (string_of_field_type field_type) ^ " option"
  | T.Repeated_field (rt, field_type, _, _,_) -> 
      (string_of_field_type field_type) ^ " " ^ (string_of_repeated_type rt)
  | T.Associative_field (at, _, (key_type, _), (value_type, _)) -> 
      Printf.sprintf "(%s * %s) %s" 
        (string_of_basic_type key_type)
        (string_of_field_type value_type) 
        (string_of_associative_type at) 
  | T.Variant_field {T.v_name; _ } -> v_name
 
(** [function_name_of_user_defined prefix user_defined] returns the function
    name of the form `(module'.'?)prefix_(type_name)`. 

    This pattern is common since a generated function for a type
    (encode/decode/to_string) will call the same generated function for each 
    user defined field type. 
 *)
let function_name_of_user_defined prefix = function 
  | {T.module_ = Some module_; T.type_name} -> 
    sp "%s.%s_%s" module_ prefix type_name
  | {T.module_ = None; T.type_name} -> 
    sp "%s_%s" prefix type_name 

let caml_file_name_of_proto_file_name proto = 
  let splitted = Util.rev_split_by_char '.' proto in 
  if List.length splitted < 2 || 
     List.hd splitted <> "proto" 
  then failwith "Proto file has no valid extension"
  else 
    String.concat "_" @@ List.rev @@ ("pb" :: (List.tl splitted)) 

let mutable_record_name s = s ^ "_mutable" 

let string_of_payload_kind ?capitalize payload_kind packed = 
  let s = match payload_kind,  packed with
  | Ocaml_types.Varint _ , false -> "varint"
  | Ocaml_types.Bits32   , false -> "bits32"
  | Ocaml_types.Bits64   , false -> "bits64"
  | Ocaml_types.Bytes    , _ -> "bytes"
  | Ocaml_types.Varint _ , true 
  | Ocaml_types.Bits32   , true
  | Ocaml_types.Bits64   , true  -> "bytes"
  in 
  match capitalize with
  | None -> s 
  | Some () -> String.capitalize s 

