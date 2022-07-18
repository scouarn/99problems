(* 47. Truth tables for logical expressions (2 variables) *)

type bool_expr =
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or  of bool_expr * bool_expr


let rec eval (bindings : (string * bool) list) (e : bool_expr) : bool =
	match e with 
	| Not(e')      -> not (eval bindings e')
	| And(e1, e2) -> (eval bindings e1) && (eval bindings e2)
	| Or (e1, e2) -> (eval bindings e1) || (eval bindings e2)
	| Var(str) ->
		let _, value = bindings |> 
			List.find (fun (str', _) -> str' = str)
		in value



let table2 (a:string) (b:string) (e:bool_expr) : (bool*bool*bool) list =

	let eval2 a_val b_val = 
		a_val, b_val, eval [a, a_val; a, b_val] e
	in [ 
		eval2 true  true;
		eval2 true  false;
		eval2 false true;
		eval2 false false; 
	]


(* 48. Truth tables for logical expressions *)

(* Intuitive version that first generates all the bindings lists and then applies eval on them *)
let rec table (vars:string list) (e:bool_expr) : ((string*bool) list * bool) list =

	let rec gen_bindings (vars : string list) : (string * bool) list list =
		let rec prefix h = List.map (fun xs -> h::xs) in

		match vars with
		| [] -> [[]]
		| h::t -> prefix (h, true)  (gen_bindings t)
		        @ prefix (h, false) (gen_bindings t)

	in gen_bindings vars |> List.map 
		(fun bindings -> bindings, eval bindings e)


(* Version that generates bindings and eval at the same time *)
let table' (vars:string list) (e:bool_expr) : ((string*bool) list * bool) list =
	let rec evaln bindings = function (*  *)
		| [] -> [List.rev bindings, eval bindings e] (* rev to match example *)
		| h::t -> 
		     evaln ((h, true )::bindings) t
		   @ evaln ((h, false)::bindings) t

	in evaln [] vars


(* 49. Gray code *)
let rec gray n = 
	if n = 1 then ["0"; "1"] else

	let rec prefix h = List.map (fun str -> h ^ str) in
	let g = gray (n-1) in
	prefix "0" g @ List.rev (prefix "1" g)


(* 50. Huffman code *)
type tree = Leaf of int * string
          | Node of int * tree * tree

let rec huffman (freqs : (string * int) list) : (string * string) list =
	
	let weight = function 
		| Leaf (f,_) | Node (f,_,_) -> f
	in

	let create_node n1 n2 = 
		Node ( (weight n1) + (weight n2), n1, n2)
	in

	let rec insert n1 = function
		| []   -> [n1]
		| n2::nodes -> if weight n1 < weight n2 
			then n1::n2::nodes
			else n2::(insert n1 nodes)
	in

	(* insertion sort to reuse insert which is also used to manage the queue in generate_tree *)
	let rec init_queue acc = function
		| [] -> acc
		| (s,f)::freqs -> 
			init_queue (insert (Leaf (f,s)) acc) freqs
	in

	let rec generate_tree = function
		| [] -> failwith "empty"
		| [node] -> node
		| n1::n2::nodes ->
			generate_tree (insert (create_node n1 n2) nodes)
	in 

	let rec unwrap_tree value = function
		| Leaf (_,str) -> [str, value]
		| Node (_,left,right) ->

		    unwrap_tree (value ^ "0") left
		  @ unwrap_tree (value ^ "1") right

	in freqs 
		|> init_queue []
		|> generate_tree
		|> unwrap_tree ""



let tests () = 
	
	assert (
		table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
		= [(true, true, true);   (true, false, true); 
		   (false, true, false); (false, false, false)]
	);


	assert (
		table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")))
			= [([("a", true);  ("b", true) ], true ); 
			   ([("a", true);  ("b", false)], true );
			   ([("a", false); ("b", true) ], false);
			   ([("a", false); ("b", false)], false)]
	);

	assert (
		table' ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")))
			= [([("a", true);  ("b", true) ], true ); 
			   ([("a", true);  ("b", false)], true );
			   ([("a", false); ("b", true) ], false);
			   ([("a", false); ("b", false)], false)]
	);


	assert (
		gray 1 = ["0"; "1"]
	);

	assert (
		gray 2 = ["00"; "01"; "11"; "10"]
	);

	assert (
		gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
	);

	assert (
		gray 4 = ["0000"; "0001"; "0011"; "0010"; "0110"; "0111"; "0101"; "0100";
		          "1100"; "1101"; "1111"; "1110"; "1010"; "1011"; "1001"; "1000"]
	);

	assert (
		let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)] in
		huffman fs = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")]
	);