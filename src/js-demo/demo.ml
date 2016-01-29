(*
  The MIT License (MIT)
  
  Copyright (c) 2016 Maxime Ransan <maxime.ransan@gmail.com>
  
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

*)

module L = Logger 
module E = Exception
module BO = Backend_ocaml
  
let caml_file_name_of_proto_file_name = Ocaml_codegen.caml_file_name_of_proto_file_name 

let wrap s = 
  if s <> "" then [s ; "\n\n" ] else [s] 

let wrap_opt = function | Some x -> wrap x | None -> []

let gen types (f:(?and_:unit -> Ocaml_types.type_ -> string))  = 
  List.flatten @@ List.rev @@ fst (List.fold_left (fun (sl, first) type_ -> 
  (if first 
  then wrap @@ f type_ 
  else wrap @@ f ~and_:() type_)::sl, false 
) ([], true) types) 
  

(* -- main -- *)

let compile_str (s:string) : string = 
  let proto  = Parser.proto_ Lexer.lexer (Lexing.from_string s) in 
  let pbtt   = Pbtt_util.compile_proto_p1 "demo.proto" proto in 
  let pbtt   = List.map (Pbtt_util.compile_proto_p2 pbtt) pbtt in 
  let grouped_pbtt = List.rev @@ Pbtt_util.group pbtt in 
  let otypes = List.rev @@ List.fold_left (fun otypes types -> 
    let l = List.flatten @@ List.map (fun t -> BO.compile pbtt t) types in 
    l :: otypes
  ) [] grouped_pbtt in 
  Util.concat @@ List.map (fun types_ -> 
    Util.concat @@ List.flatten [
      gen      types_ Ocaml_codegen.gen_type;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_decode_sig t) types_ ;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_encode_sig t) types_ ;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_pp_sig t) types_ ;
      List.flatten @@ List.map (fun t -> wrap_opt @@ Ocaml_codegen.gen_default_sig t) types_ ;
    ]
  ) (otypes)

let () = 
  print_endline @@ compile_str "message x { required int32 f = 1; }"  
