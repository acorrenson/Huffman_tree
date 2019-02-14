
(* the description of an emitter u *)
let u = [('a', 0.5); ('b', 0.2); ('c', 0.2); ('d', 0.1)]


type codetree =
  | Node of float * codetree * codetree
  | Leaf of char * float


let print_emitter e =
  (* print the description of an emitter *)
  let read (a, b) =
    
    print_char a;
    print_char ' ';
    print_float b;
    print_newline ()
  in
  List.iter read e


let tuple_of_node node =
  match node with
  | Node (a, b, c) -> a, b, c
  | _ -> failwith "error"


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

let find_min_freq emitter =
  let rec find_min_freq_rec emitter (key1, min1) (key2, min2) =
    match emitter with
    | [] -> 
      ((key1, min1), (key2, min2))

    | (key, freq)::t when freq <= min1 ->
        find_min_freq_rec t (key, freq) (key1, min1)

    | (key, freq)::t when freq <= min2 ->
        find_min_freq_rec t (key1, min1) (key, freq)

    | h::t ->
      find_min_freq_rec t (key1, min1) (key2, min2)

  in

  find_min_freq_rec emitter (' ', 1.0) (' ', 1.0)


let remove c l =
  let test e = 
    match e with
    | (a, b) when a == c -> false
    | _ -> true
  in
  List.filter test l


let huffman emitter =
  let rec huffman_rec emitter node =
    match emitter with
    | [] -> node
    | h::t as l ->
      let (key1, freq1), (_, _) = find_min_freq l in
      let new_emitter = remove key1 l in (* remove min freq *)
      let (freq2, _, _) = tuple_of_node node in
      let new_node = Node (freq1 +. freq2, Leaf (key1, freq1), node) in
      huffman_rec new_emitter new_node
  in
  
  (* init the first sub-tree *)
  let ((k1, f1), (k2, f2)) = find_min_freq emitter in
  let em1 = remove k1 emitter in (* remove min freq 1 *)
  let em2 = remove k2 em1 in (* remove min freq 2 *)
  let n = Node (f1 +. f2, Leaf (k1, f1), Leaf (k2, f2)) in

  (* build the rest of the tree *)
  huffman_rec em2 n

let _ =
  print_endline "description of the emitter :";
  print_emitter u;
  print_endline "\nCode Tree \n";
  pprint (huffman u) 0;


