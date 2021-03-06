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

let rev_split_by_char c s = 
  let rec loop i l = 
    try 
      let i' = String.index_from s i c  in 
      let s' = String.sub s i (i' - i)  in 
      loop (i'+1) (if s' = "" then l else s'::l)  
    with Not_found -> (String.sub s i (String.length s - i) ):: l 
  in 
  loop 0 []
  
(** [concat l] concatenate a string list *)
let concat = String.concat ""

let rec pop_last = function 
  | [] -> failwith "Invalid argument [] for pop_last"
  | hd::[] -> []
  | hd::tl -> hd :: (pop_last tl)

let rec apply_until f = function 
  | []  -> None 
  | hd::tl -> (match f hd with 
    | None -> apply_until f tl 
    | x    -> x
  )  

let is_list_empty = function | [] -> true | _ -> false 

let string_of_string_list l = 
  Printf.sprintf "[%s]" (String.concat "," l)

let string_fold_lefti f e0 s =
  let len = String.length s in 
  let rec loop acc = function
    | i when i = len -> acc 
    | i -> loop (f acc i (String.unsafe_get s i))  (i + 1) 
  in 
  loop e0 0 

let option_default x = function
  | Some y -> y 
  | None -> x 

