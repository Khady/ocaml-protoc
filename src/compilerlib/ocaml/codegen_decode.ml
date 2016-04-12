
module T   = Ocaml_types 
module E   = Exception 
module F   = Fmt

open Codegen_util 

let decode_basic_type bt pk = 
  Backend_ocaml_static.runtime_function (`Decode, pk, bt)
  
let decode_field_f field_type pk = 
  match field_type with 
  | T.User_defined_type t -> 
      let f_name = function_name_of_user_defined "decode" t in
      if t.T.nested 
      then (f_name ^ " (Pbrt.Decoder.nested d)")
      else (f_name ^ " d") 
  | T.Unit -> 
      "Pbrt.Decoder.empty_nested d"
  | T.Basic_type bt -> (decode_basic_type bt pk) ^ " d" 

let gen_decode_record ?and_ {T.r_name; r_fields} sc = 

  (* list fields have a special treatement when decoding since each new element
     of a repeated field is appended to the front of the list. In order
     to retreive the right order efficiently we reverse all the repeated field
     lists values when the message is done being decoded. 
   *) 
  let all_lists = List.fold_left (fun acc {T.rf_label; rf_field_type; _ } -> 
    match rf_field_type with
    | T.Repeated_field (T.Rt_list, _, _ , _, _ ) -> rf_label :: acc 
    | T.Associative_field (T.Al_list, _, _, _) -> rf_label :: acc
    | _ -> acc  
  ) [] r_fields in  

  (* let all_lists = [] in
   *)

  let string_of_nonpacked_pk pk = 
    Codegen_util.string_of_payload_kind ~capitalize:() pk false 
  in 

  let process_field_common sc encoding_number pk_as_string f = 
    F.line sc @@ sp "| Some (%i, %s) -> (" encoding_number pk_as_string; 
    F.scope sc (fun sc -> 
      f sc;
      F.line sc "loop ()";
    );
    F.line sc ")"
  in

  let process_required_field sc rf_label (field_type, encoding_number, pk, _) = 
    process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
      F.line sc @@ sp "v.%s <- %s" rf_label (decode_field_f field_type pk); 
    ) 
  in

  let process_optional_field sc rf_label (field_type, encoding_number, pk, _) = 
    process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
      F.line sc @@ sp "v.%s <- Some (%s);" rf_label (decode_field_f field_type pk); 
    )
  in

  let process_repeated_field sc rf_label (rt, field_type, encoding_number, pk, is_packed) = 
    match rt, is_packed with
    | T.Rt_list, false -> 
      process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
        F.line sc @@ sp "v.%s <- (%s) :: v.%s;" rf_label (decode_field_f field_type pk) rf_label; 
      ) 
    | T.Rt_repeated_field, false -> (
      process_field_common sc encoding_number (string_of_nonpacked_pk pk) (fun sc -> 
        F.line sc @@ sp "Pbrt.Repeated_field.add (%s) v.%s; " rf_label (decode_field_f field_type pk); 
      ) 
    ) 
    | T.Rt_list, true -> (
      process_field_common sc encoding_number "Bytes" (fun sc -> 
        F.line sc @@ sp "v.%s <- Pbrt.Decoder.packed_fold (fun l d -> (%s)::l) [] d" 
          rf_label (decode_field_f field_type pk) 
      ) 
    ) 
    | T.Rt_repeated_field, true -> (
      process_field_common sc encoding_number "Bytes" (fun sc -> 
        F.line sc "Pbrt.Decoder.packed_fold (fun () d -> ";
        F.scope sc (fun sc -> 
          F.line sc @@ sp "Pbrt.Repeated_field.add (%s) v.%s" (decode_field_f field_type pk) rf_label;
        );
        F.line sc       ") [] d";
      ) 
    ) 
  in

  let process_associative_field sc rf_label (_, encoding_number, (key_type, key_pk), (value_type, value_pk)) = 
    let decode_key_f   = decode_basic_type key_type key_pk in 
    let decode_value_f = decode_field_f value_type value_pk in 
      (* TODO This won't work, you actually need to build a more complex function to 
       * decode the nested types. 
       *)
    process_field_common sc encoding_number "Bytes" (fun sc -> 
      F.line sc @@ sp "v.%s <- (" rf_label; 
      F.scope sc (fun sc -> 
        F.line sc @@ sp "Pbrt.Decoder.map_entry d ~decode_key:%s ~decode_value:%s)::v.%s"
          decode_key_f decode_value_f rf_label
      ); 
      F.line sc ");" 
    )
  in

  let process_variant_field sc rf_label {T.v_constructors; _} = 
    List.iter (fun {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind = pk; } -> 
      process_field_common sc vc_encoding_number (string_of_nonpacked_pk pk) (fun sc->  
        match vc_field_type  with
        | T.Nullary -> (
          F.line sc "Pbrt.Decoder.empty_nested d;";
          F.line sc @@ sp "v.%s <- %s;" rf_label vc_constructor; 
        ) 
        | T.Non_nullary_constructor field_type -> (
          F.line sc @@ sp "v.%s <- %s (%s)" 
            rf_label vc_constructor (decode_field_f field_type pk)
        ) 
      )
    ) v_constructors
  in 

  let mutable_record_name = Codegen_util.mutable_record_name r_name in 

  F.line sc @@ sp "%s decode_%s d =" (let_decl_of_and and_) r_name; 
  F.scope sc (fun sc -> 
    F.line sc @@ sp "let v = default_%s () in" mutable_record_name;
    F.line sc "let rec loop () = "; 
    F.scope sc (fun sc -> 
      F.line sc "match Pbrt.Decoder.key d with";
      F.line sc "| None -> (";
      F.scope sc (fun sc -> 
        List.iter (fun field_name -> 
          F.line sc @@ sp "v.%s <- List.rev v.%s;" field_name field_name
        ) all_lists;   
      );
      F.line sc ")";
      List.iter (fun {T.rf_label; rf_field_type; _ } -> 
        match rf_field_type with
        | T.Required x -> process_required_field sc rf_label x 
        | T.Optional x -> process_optional_field sc rf_label x 
        | T.Repeated_field x -> process_repeated_field sc rf_label x 
        | T.Associative_field x-> process_associative_field sc rf_label x 
        | T.Variant_field x -> process_variant_field sc rf_label x 
      ) r_fields; 
      F.line sc "| Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()";
    ); 
    F.line sc "in"; 
    F.line sc "loop ();";
    F.line sc @@ sp "let v:%s = Obj.magic v in" r_name; 
    F.line sc "v";
  )

let gen_decode_variant ?and_ {T.v_name; v_constructors;} sc = 

  let process_ctor sc {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind = pk ; } = 
    match vc_field_type with 
    | T.Nullary -> (
      F.line sc @@ sp "| Some (%i, _) -> (Pbrt.Decoder.empty_nested d ; %s)" vc_encoding_number vc_constructor
    ) 
    | T.Non_nullary_constructor field_type -> begin match field_type with

      | T.User_defined_type ud -> 
        let f_name = function_name_of_user_defined "decode" ud in
        let decoding = 
          if ud.T.nested 
          then (f_name ^ " (Pbrt.Decoder.nested d)")
          else (f_name ^ " d") in
        F.line sc @@ sp "| Some (%i, _) -> %s (%s)" vc_encoding_number vc_constructor decoding 

      | T.Unit -> assert(false)
      
      | T.Basic_type bt -> 
        F.line sc @@ sp "| Some (%i, _) -> %s (%s)" 
          vc_encoding_number vc_constructor (decode_basic_type bt pk) 
    end 
  in 

  F.line sc @@ sp "%s decode_%s d = " (let_decl_of_and and_) v_name;
  F.scope sc (fun sc ->
    F.line sc @@ sp "let rec loop () = "; 
    F.scope sc (fun sc ->
      F.line sc @@ sp "let ret:%s = match Pbrt.Decoder.key d with" v_name;
      F.scope sc (fun sc -> 
        F.line sc "| None -> failwith \"None of the known key is found\"";
        List.iter (fun ctor -> process_ctor sc ctor) v_constructors; 
        F.line sc "| Some (n, payload_kind) -> (";
        F.line sc "  Pbrt.Decoder.skip d payload_kind; ";
        F.line sc "  loop () ";
        F.line sc ")";
      );
      F.line sc "in";
      F.line sc "ret";
    ); 
    F.line sc "in"; 
    F.line sc "loop ()";
  )

let gen_decode_const_variant ?and_ {T.cv_name; cv_constructors; } sc = 

  F.line sc @@ sp "%s decode_%s d = " (let_decl_of_and and_) cv_name; 
  F.scope sc (fun sc -> 
    F.line sc "match Pbrt.Decoder.int_as_varint d with";
    List.iter (fun (name, value) -> 
      F.line sc @@ sp "| %i -> (%s:%s)" value name cv_name
    ) cv_constructors; 
    F.line sc @@ sp "| _ -> failwith \"Unknown value for enum %s\"" cv_name
  )

let gen_struct ?and_ t sc = 
  let (), has_encoded =  match t with 
    | {T.spec = T.Record r; _ }  -> gen_decode_record ?and_ r sc, true
    | {T.spec = T.Variant v; _ } -> gen_decode_variant ?and_ v sc, true
    | {T.spec = T.Const_variant v; _ } -> gen_decode_const_variant ?and_ v sc, true
  in
  has_encoded

let gen_sig ?and_ t sc = 

  let f type_name = 
    F.line sc @@ sp "val decode_%s : Pbrt.Decoder.t -> %s" type_name type_name ; 
    F.line sc @@ sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)" type_name type_name; 
  in 

  let (), has_encoded = match t with 
    | {T.spec = T.Record {T.r_name; _ } } -> f r_name, true
    | {T.spec = T.Variant {T.v_name; _ }} -> f v_name, true 
    | {T.spec = T.Const_variant {T.cv_name; _ }} -> f cv_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Decoding"
