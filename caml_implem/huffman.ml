(* -----------------------------------------------*)
(* ---- Information Theory - Huffman Trees -------*)
(* -----------------------------------------------*)


(* the description of 3 emitters u, v, w *)
let u = [
  ('a', 0.5);
  ('b', 0.2);
  ('c', 0.2);
  ('d', 0.1)
  ]
let v = [
  (' ', 2.);
  ('e', 3.);
  ('i', 1.);
  ('n', 1.);
  ('p', 1.);
  ('t', 4.);
  ('u', 1.);
  ('x', 1.);
  ]
let w = [
  ('a', 0.2);
  ('b', 0.15);
  ('c', 0.4);
  ('d', 0.15);
  ('e', 0.1);
]

(* typedef for Huffman Tree *)
type htree =
  | Node of float * htree * htree
  | Leaf of char * float


(* -----------------------------------------------*)
(* ---- Print the description of an emitter e ----*)
(* -----------------------------------------------*)

let print_emitter e =
  let read (a, b) =
    
    print_char a;
    print_char ' ';
    print_float b;
    print_newline ()
  in
  List.iter read e


(* ----------------------------------- *)
(* ---- Functions to print tree ------ *)
(* ----------------------------------- *)

let print_pad i s =
  let pad = String.make (i*2) ' ' in
  print_string (pad ^ s)
  

let rec pprint tree i =
  match tree with
  | Node (a, b, c) -> 
    print_pad i (string_of_float a);
    print_newline ();
    pprint b (i+1);
    pprint c (i+1)
    
  | Leaf (a, b) ->
    print_pad i ("(" ^ (String.make 1 a)^ "  " ^ (string_of_float b) ^ ")");
    print_newline ()


(* ------------------------------------------------ *)
(* ---- Functions to construct Huffman Trees ------ *)
(* ------------------------------------------------ *)

let init_tree_set u =
  let rec add_tree_to_list l u =
    match u with
    | [] -> l
    | (a, b)::t ->
      add_tree_to_list ((Leaf (a, b))::l) t
    in 
    add_tree_to_list [] u


let combine_two t1 t2 =
  match t1, t2 with
  | Leaf (_, f1), Leaf (_, f2) -> Node (f1 +. f2, t2, t1)
  | Leaf (_, f1), Node (f2, _, _) -> Node (f1 +. f2, t2, t1)
  | Node (f1, _, _), Leaf (_, f2) -> Node (f1 +. f2, t2, t1)
  | Node (f1, _, _), Node (f2, _, _) -> Node (f1 +. f2, t2, t1)
  

let compare_tree t1 t2 =
  match t1, t2 with
  | Node (a, b, c), Node (d, e, f) ->
    int_of_float (100. *. (a -. d))
  | Node (a, b, c), Leaf (d, e) ->
    int_of_float (100. *. (a -. e))
  | Leaf (a, b), Leaf (c, d) ->
    int_of_float (100. *. (b -. d))
  | Leaf (a, b), Node (c, d, e) ->
    int_of_float (100. *. (b -. c))


let rec print_list l =
  match l with
  | [] -> print_newline ()
  | Node (a, b, c)::t -> print_float a; print_char ' '; print_list t
  | Leaf (a, b)::t -> print_float b; print_char ' '; print_list t


let huffman u =
  let rec combine_all l =
    match l with
    | h::[] -> h
    | _ as l ->
      let sorted = List.sort compare_tree l in
      let ht = combine_two (List.nth sorted 0) (List.nth sorted 1) in
      let new_l = ht::(List.tl (List.tl sorted)) in
      combine_all new_l
  in
  let tree_set = init_tree_set u in
  combine_all tree_set


let _ =
  print_endline "description of the emitter u:";
  print_emitter u;
  print_endline "description of the emitter v: (freq. are mutiplied by 14)";
  print_emitter v;
  print_endline "description of the emitter w:";
  print_emitter w;
  print_endline "Optimized coding tree for the emitter u:";
  pprint (huffman u) 0;
  print_endline "Optimized coding tree for the emitter v:";
  pprint (huffman v) 0;
  print_endline "Optimized coding tree for the emitter w:";
  pprint (huffman w) 0;