
module T = Ocaml_types
module F = Fmt 
module E = Exception
module L = Logger

open Codegen_util

let constructor_name s =
  String.capitalize @@ String.lowercase s 

let gen_encode_field_key sc number pk is_packed = 
  F.line sc @@ sp "Pbrt.Encoder.key (%i, Pbrt.%s) encoder; " 
    number (constructor_name @@ Codegen_util.string_of_payload_kind pk is_packed)

let gen_encode_field_type ?with_key sc var_name encoding_number pk is_packed field_type = 

  let encode_basic_type bt = 
    Backend_ocaml_static.runtime_function (`Encode, pk, bt) 
  in
  
  let encode_key sc = 
    match with_key with
    | Some () -> gen_encode_field_key sc encoding_number pk is_packed 
    | None -> ()
  in 

  match field_type with 
  | T.User_defined_type ud -> (
    encode_key sc;
    let f_name = function_name_of_user_defined "encode" ud in 
    if ud.T.nested 
    then F.line sc @@ sp "Pbrt.Encoder.nested (%s %s) encoder;" f_name var_name
    else F.line sc @@ sp "%s %s encoder;" f_name var_name
  )
  | T.Unit -> (
    encode_key sc;
    F.line sc "Pbrt.Encoder.empty_nested encoder;" 
  )
  | T.Basic_type bt -> ( 
    encode_key sc;
    let rt = encode_basic_type bt in 
    F.line sc @@ sp "%s %s encoder;" rt var_name
  )
  (* 
  | T.Associative_list {T.al_key; al_value; }, _ -> ( 
    let encode_key_f = encode_basic_type al_key in 
    let encode_value_f = begin match al_value with
      | T.Al_basict_type bt -> encode_basic_type bt 
      | T.Al_user_defined_type ud -> function_name_of_user_defined "encode" ud 
    end in 

    F.line sc "List.iter (fun (k, v) ->"; 
    F.scope sc (fun sc -> 
      encode_key sc; 
      F.line sc @@ "Pbrt.Encoder.map_entry";
      F.scope sc (fun sc -> 
        F.line sc @@ sp "~encode_key:%s" encode_key_f;  
        F.line sc @@ sp "~encode_value:%s" encode_value_f;  
        F.line sc @@ sp "((k, %s), (v, %s))" "Pbrt.Bytes" "Pbrt.Bytes"; 
        (* TODO fix hardcode payload kind*)
        F.line sc "encoder"; 
      ); 
    ); 
    F.line sc @@ sp ") %s;" v_name; 
  )
 *)

let gen_encode_record ?and_ {T.r_name; r_fields } sc = 
  L.log "gen_encode_record record_name: %s\n" r_name;

  let rn = r_name in 
  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " (let_decl_of_and and_) rn rn;
  F.scope sc (fun sc -> 
    List.iter (fun record_field -> 
      let {T.rf_label; rf_field_type; _ } = record_field in  

      match rf_field_type with 
      | T.Required (field_type, encoding_number, pk, _) -> ( 
        let var_name = sp "v.%s" rf_label in 
        gen_encode_field_type ~with_key:() sc var_name encoding_number pk false (* packed *) field_type
      )
      | T.Optional (field_type, encoding_number, pk, _) -> (
        F.line sc "(";
        F.scope sc (fun sc -> (
          F.line sc @@ sp "match v.%s with " rf_label;
          F.line sc @@ sp "| Some x -> (";
          F.scope sc (fun sc ->
            gen_encode_field_type ~with_key:() sc "x" encoding_number pk false field_type;
          ); 
          F.line sc ")";
          F.line sc "| None -> ();";
        ); 
        F.line sc ")";
        )
      )
      | T.Repeated_field (rt, field_type, encoding_number, pk, is_packed) -> (
        match rt, is_packed with
        | T.Rt_list, false -> (
          F.line sc "List.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field_type ~with_key:() sc "x" encoding_number pk is_packed field_type;
          );
          F.line sc @@ sp ") v.%s;" rf_label; 
        ) 
        | T.Rt_repeated_field, false -> ( 
          F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
          F.scope sc (fun sc -> 
            gen_encode_field_type ~with_key:() sc "x" encoding_number pk is_packed field_type;
          );
          F.line sc @@ sp ") v.%s;" rf_label; 
        ) 
        | T.Rt_list, true -> (
          gen_encode_field_key sc encoding_number pk is_packed;
            (* When packed the key is encoded once. 
             *)
          F.line sc "Pbrt.Encoder.nested (fun encoder ->";
          F.scope sc (fun sc -> 
            F.line sc "List.iter (fun x -> ";
            F.scope sc (fun sc -> 
              gen_encode_field_type sc "x" encoding_number pk is_packed field_type;
            );
            F.line sc @@ sp ") v.%s;" rf_label; 
          );
          F.line sc ") encoder;";
        ) 
        | T.Rt_repeated_field, true -> (
          gen_encode_field_key sc encoding_number pk is_packed;
            (* When packed the key is encoded once. 
             *)
          F.line sc "Pbrt.Encoder.nested (fun encoder ->";
          F.scope sc (fun sc -> 
            F.line sc "Pbrt.Repeated_field.iter (fun x -> ";
            F.scope sc (fun sc -> 
              gen_encode_field_type sc "x" encoding_number pk is_packed field_type;
            );
            F.line sc @@ sp ") v.%s;" rf_label; 
          );
          F.line sc") encoder;";
        ) 
      ) 
      | T.Variant_field {T.v_name; v_constructors; } -> (
        F.line sc  "(";
        F.scope sc (fun sc -> 
          F.line sc @@ sp "match v.%s with" rf_label;
          List.iter (fun {T.vc_constructor; vc_field_type; vc_encoding_number; vc_payload_kind} -> 
            begin match vc_field_type with
            | T.Nullary -> ( 
              F.line sc @@ sp "| %s -> (" vc_constructor; 
              F.scope sc (fun sc -> 
                gen_encode_field_key sc vc_encoding_number vc_payload_kind false;
                F.line sc "Pbrt.Encoder.empty_nested encoder";
              );
              F.line sc ")";
            )
            | T.Non_nullary_constructor field_type -> (
              F.line sc @@ sp "| %s x -> (" vc_constructor;
              F.scope sc (fun sc -> 
                gen_encode_field_type sc ~with_key:() "x" vc_encoding_number vc_payload_kind false field_type  
              ); 
              F.line sc  ")";
            )
            end;
          ) v_constructors;
        );
        F.line sc ")"
      ) 
      
      (*
      | T.One_of {T.variant_constructors; T.variant_name = _; T.variant_encoding = T.Standalone} -> (  
        E.programmatic_error E.One_of_should_be_inlined_in_message
      )
      *)
    ) r_fields (* List.iter *); 
    F.line sc "()";
  ) (* encode function *)

let gen_encode_variant ?and_ variant sc = 
  let {T.variant_name; T.variant_constructors; T.variant_encoding = _ } = variant in  
  F.line sc @@ sp "%s encode_%s (v:%s) encoder = " (let_decl_of_and and_) variant_name variant_name;
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun {T.field_name; T.field = {T.encoding; field_type; _}} ->
      (match field_type with 
      | T.Unit -> F.line sc @@ sp "| %s -> (" field_name
      | _ -> F.line sc @@ sp "| %s x -> (" field_name
      );
      F.scope sc (fun sc -> 
        gen_encode_field_key sc encoding;
        gen_encode_field sc "x" encoding field_type
      ); 
      F.line sc ")";
    ) variant_constructors;
  ) 

let gen_encode_const_variant ?and_ {T.cvariant_name; T.cvariant_constructors; } sc = 
  F.line sc @@ sp "%s encode_%s (v:%s) encoder =" (let_decl_of_and and_) cvariant_name cvariant_name; 
  F.scope sc (fun sc -> 
    F.line sc "match v with";
    List.iter (fun (name, value) -> 
      F.line sc (
        if value > 0 
        then sp "| %s -> Pbrt.Encoder.int_as_varint %i encoder" name value
        else sp "| %s -> Pbrt.Encoder.int_as_varint (%i) encoder" name value
      )
    ) cvariant_constructors; 
  )

let gen_struct ?and_ t sc  = 
  let (), has_encoded = match t with 
    | {T.spec = T.Record r } -> gen_encode_record  ?and_ r sc, true
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with 
      | T.Standalone -> gen_encode_variant ?and_ v sc, true
      | _ -> (), false 
    )
    | {T.spec = T.Const_variant v } ->
      gen_encode_const_variant ?and_ v sc, true
  in 
  has_encoded

let gen_sig ?and_ t sc = 
  let f type_name = 
    F.line sc @@ sp "val encode_%s : %s -> Pbrt.Encoder.t -> unit" type_name type_name;
    F.line sc @@ sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" type_name; 
  in 
  let (), has_encoded = match t with 
    | {T.spec = T.Record {T.record_name ; _ } }-> f record_name, true
    | {T.spec = T.Variant v } -> (match v.T.variant_encoding with
      | T.Standalone -> f v.T.variant_name, true 
      | T.Inlined_within_message -> (), false
    ) 
    | {T.spec = T.Const_variant {T.cvariant_name; _ } } -> f cvariant_name, true
  in
  has_encoded

let ocamldoc_title = "Protobuf Toding"
