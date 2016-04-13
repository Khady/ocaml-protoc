
module T = Ocaml_types
module F = Fmt

open Codegen_util

let type_decl_of_and = function | Some () -> "and" | None -> "type" 

let gen_type_record ?mutable_ ?and_ {T.r_name; r_fields } sc = 

  let mutable_ = match mutable_ with
    | Some () -> true
    | None    -> false 
  in 
  
  let field_prefix field_type field_mutable = 
    if field_mutable 
    then "mutable"
    else match field_type with
      | T.Required _ 
      | T.Optional _ 
      | T.Variant_field _ -> if mutable_ then  "mutable " else ""
      | T.Repeated_field (rt, _, _, _, _) -> begin match rt with
        | T.Rt_repeated_field -> ""
        | T.Rt_list -> if mutable_ then "mutable " else ""
      end 
      | T.Associative_field (at, _, _, _) -> begin match at with
        | T.Al_list -> if mutable_ then "mutable " else ""
      end 
  in

  let r_name = 
    if mutable_ 
    then Codegen_util.mutable_record_name r_name 
    else r_name 
  in 

  F.line sc @@ sp "%s %s = {" (type_decl_of_and and_) r_name;
  F.scope sc (fun sc -> 
    List.iter (fun {T.rf_label; rf_field_type; rf_mutable;} ->  
      let prefix = field_prefix rf_field_type rf_mutable in 
      let type_string = Codegen_util.string_of_record_field_type rf_field_type in 
      F.line sc @@ sp "%s%s : %s;" prefix rf_label type_string 
    ) r_fields;
  ); 
  F.line sc "}"

let gen_type_variant ?and_ variant sc =  
  let {T.v_name; v_constructors; } = variant in

  F.line sc @@ sp "%s %s =" (type_decl_of_and and_) v_name; 

  F.scope sc (fun sc -> 
    List.iter (fun {T.vc_constructor; vc_field_type; _} ->
      match vc_field_type with
      | T.Nullary -> F.line sc @@ sp "| %s" vc_constructor
      | T.Non_nullary_constructor  field_type -> (
        let type_string = string_of_field_type field_type in 
        F.line sc @@ sp "| %s of %s" vc_constructor type_string 
      )
    ) v_constructors;
  )

let gen_type_const_variant ?and_ {T.cv_name; cv_constructors} sc = 
  F.line sc @@ sp "%s %s =" (type_decl_of_and and_) cv_name; 
  F.scope sc (fun sc -> 
    List.iter (fun (name, _ ) -> 
      F.line sc @@ sp "| %s " name
    ) cv_constructors;
  )

let gen_struct ?and_ t scope = 
  begin
    match t with 
    | {T.spec = T.Record r; _ } -> (
      gen_type_record  ?and_ r scope; 
      F.empty_line scope;
      gen_type_record ~mutable_:() ~and_:() r scope 
    ) 
    | {T.spec = T.Variant v; _ } -> gen_type_variant  ?and_ v scope  
    | {T.spec = T.Const_variant v; _ } -> gen_type_const_variant ?and_ v scope 
  end; 
  true

let gen_sig ?and_ t scope = 
  begin
    match t with 
    | {T.spec = T.Record r; _ } -> (
      gen_type_record  ?and_ r scope 
    ) 
    | {T.spec = T.Variant v; _ } -> gen_type_variant  ?and_ v scope  
    | {T.spec = T.Const_variant v; _ } -> gen_type_const_variant ?and_ v scope 
  end; 
  true

let ocamldoc_title = "Types"
