(* -----------------------------------------------*)
(* ---- Information Theory - Huffman Trees -------*)
(* -----------------------------------------------*)


(* the description of an emitter u *)
let u = [('a', 0.5); ('b', 0.2); ('c', 0.2); ('d', 0.1)]


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
(* ---- Conversion htree -> tuple ---- *)
(* ----------------------------------- *)

let tuple_of_node node =
  match node with
  | Node (a, b, c) -> a, b, c
  | _ -> failwith "error node"


let tuple_of_leaf leaf =
  match leaf with
  | Leaf (a, b) -> a, b
  | _ -> failwith "error leaf"


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
  | Leaf (_, f1), Leaf (_, f2) -> Node (f1 +. f2, t1, t2)
  | Leaf (_, f1), Node (f2, _, _) -> Node (f1 +. f2, t1, t2)
  | _ -> failwith "error"
  

let compare_leaf l1 l2 =
  let (_, f1) = tuple_of_leaf l1 in
  let (_, f2) = tuple_of_leaf l2 in
  int_of_float (f1 -. f2)


let huffman u =
  let rec combine_all ht l =
    match l with
    | [] -> ht
    | h::t -> combine_all (combine_two h ht) t
  in
  let tree_set = init_tree_set u in
  let sorted = List.sort compare_leaf tree_set in
  let ht = combine_two (List.nth sorted 0) (List.nth sorted 1) in
  combine_all ht (List.tl (List.tl sorted))


let _ =
  print_endline "description of the emitter :";
  print_emitter u;
  print_endline "Optimized coding tree for the emitter :";
  pprint (huffman u) 0