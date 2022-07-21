type 'a binary_tree =
	| Empty
	| Node of 'a * 'a binary_tree * 'a binary_tree

let rec cartesian value xs ys =
	match xs, ys with
	| [], _ | _, [] -> []
	| x::xs, y::ys ->
		Node (value, x, y) :: cartesian value xs ys
		@ cartesian value [x] ys
		@ cartesian value xs [y]


(* 55. Construct completely balanced binary trees *)
let rec cbal_tree (n : int) : char binary_tree list = 

	match n with
	| 0 -> [Empty]
	| n when n mod 2 = 1 -> 
		let subtrees = cbal_tree ((n-1) / 2) in
		cartesian 'x' subtrees subtrees 

	| n -> 
		let t1 = cbal_tree ((n-1) / 2) in
		let t2 = cbal_tree ((n-1) - (n-1) / 2) in
		  cartesian 'x' t1 t2 
		@ cartesian 'x' t2 t1 


(* cant bother having the same order *)
let multiset_eq (xs : 'a list) (ys : 'a list) : bool = 
	let count n = List.fold_left (fun acc x -> acc + if n = x then 1 else 0) 0 in
	xs |> List.for_all (fun x -> count x xs = count x ys) &&
	ys |> List.for_all (fun y -> count y ys = count y xs)

let (=$=) = multiset_eq



(* 56. Symmetric binary trees *)
let rec is_symmetric (tree : 'a binary_tree) : bool = 
	
	let rec is_symmetric_of = function 
		| Empty, Empty -> true
		| _, Empty | Empty, _ -> false
		| Node (x, a, b), Node (y, c, d) -> 
			is_symmetric_of (a, d) && is_symmetric_of (b, c)

	in is_symmetric_of (tree, tree)


(* 57. Binary search trees (dictionaries) *)
let rec construct (xs : 'a list) : 'a binary_tree = 

	let rec partition p = function
		| []   -> [], []
		| x::xs -> 
			let ys, zs = partition p xs in
			if x < p then x::ys, zs else ys, x::zs 
	in

	match xs with 
	| [] -> Empty
	| x::xs -> 
		let ys, zs = partition x xs in
		Node (x, construct ys, construct zs)


(* 58. Generate-and-test paradigm *)
let sym_cbal_tree (n : int) : char binary_tree list = 
	cbal_tree n |> List.filter is_symmetric

let rec nb_bal_tree (n : int) : int =
	match n with
	| 0 -> 1
	| 1 -> 1 
	| n when n mod 2 = 1 -> 
		let m = nb_bal_tree ((n-1) / 2) in m * m
	| n ->
		let t1 = nb_bal_tree ((n-1) / 2)
		and t2 = nb_bal_tree ((n-1) - (n-1)/2) in
		t1 * t2 * 2 


let nb_sym_bal_tree (n : int) : int = 
	if n mod 2 = 0 then 0 else nb_bal_tree ((n-1) / 2)


(* 59. Construct height-balanced binary trees *)
let rec hbal_tree (n : int) : char binary_tree list =
	match n with
	| 0 -> [Empty]
	| 1 -> [Node ('x', Empty, Empty)]
	| n ->
		let t1 = hbal_tree (n-1)
		and t2 = hbal_tree (n-2) in
		  cartesian 'x' t1 t1
		@ cartesian 'x' t1 t2
		@ cartesian 'x' t2 t1


(* 60. Construct height-balanced binary trees with a given number of nodes *)
let max_nodes (n : int) : int = 
	1 lsl n - 1 (* 2^n - 1 *)


let rec nb_nodes = function
	| Empty -> 0
	| Node (_, lhs, rhs) -> 
		1 + nb_nodes lhs + nb_nodes rhs


let min_nodes_actual (n : int) : int =
	match hbal_tree n with
	| [] -> -1
	| hd::tl -> 
		List.fold_left (fun acc x -> min acc (nb_nodes x)) (nb_nodes hd) tl


let rec min_nodes (n : int) : int =
	match n with
	| 0 -> 0
	| 1 -> 1
	| n -> 
		1 + (min_nodes (n-1)) + (min_nodes (n-2))


let rec min_height (n : int) : int =
	match n with
	| 0 -> 0
	| 1 -> 1
	| n -> 
		let t1 = min_height ((n-1)/2)
		and t2 = min_height ((n-1) - (n-1)/2) in
		1 + max t1 t2

(* TODO *)



(* 61. Collect the leaves of a binary tree in a list *)
let rec leaves (tree : 'a binary_tree) : 'a list =
	match tree with
	| Empty -> []
	| Node (x, Empty, Empty) -> [x]
	| Node (_, lhs, rhs) -> 
		leaves lhs @ leaves rhs


(* 62. Count the leaves of a binary tree *)
let rec count_leaves (tree : 'a binary_tree) : int = 
	match tree with
	| Empty -> 0
	| Node (_, Empty, Empty) -> 1
	| Node (_, lhs, rhs) -> 
		count_leaves lhs + count_leaves rhs


(* 63. Collect the nodes at a given level in a list *)
let rec at_level (tree : 'a binary_tree) (n : int) : 'a list =
	match tree with
	| Empty -> []
	| Node (x, _, _) when n <= 1 -> [x]
	| Node (_, lhs, rhs) ->
		at_level lhs (n-1) @ at_level rhs (n-1) 


(* 64. Collect the internal nodes of a binary tree in a list *)
let rec internals (tree : 'a binary_tree) : 'a list =
	match tree with
	| Empty -> []
	| Node (_, Empty, Empty) -> []
	| Node (x, lhs, rhs) -> x :: internals lhs @ internals rhs


(* 65. Construct a complete binary tree *)
(* TODO *)


(* 66. Layout a binary tree (1) *)
(* TODO *)


(* 67. Layout a binary tree (2) *)
(* TODO *)


(* 68. Layout a binary tree (3) *)
(* TODO *)


(* 69. A string representation of binary trees *)
let rec string_of_tree (tree : char binary_tree) : string = 
	match tree with
	| Empty -> ""
	| Node (c, lhs, rhs) ->	
		String.make 1 c      ^ "(" 
		^ string_of_tree lhs ^ ","
		^ string_of_tree rhs ^ ")"


let tree_of_string (str : string) : char binary_tree = 

	let rec parse_head i = match str.[i] with
		| '(' -> failwith "head : token ("
		| ')' -> failwith "head : token )"
		| ',' -> failwith "head : token ,"
		|  c  ->

		match str.[i+1] with
		| ')' | ',' -> Node (c, Empty, Empty), i+1
		| '(' -> 
			let lhs, j = parse_lhs (i+1) in
			let rhs, k = parse_rhs (j+1) in
			Node (c, lhs, rhs), k

		| _ -> failwith "Ident"

	and parse_lhs i = match str.[i] with
		| '(' -> parse_lhs (i+1)
		| ')' -> failwith "lhs : token )"
		| ',' -> Empty, i
		|  c  -> parse_head i 

	and parse_rhs i = match str.[i] with
		| '(' -> failwith "rhs : token ("
		| ',' -> parse_rhs (i+1)
		| ')' -> Empty, i
		|  c  -> parse_head i


	in let tree, i = parse_head 0 in tree


(* 70. Preorder and inorder sequences of binary trees *)
let rec preorder (tree : 'a binary_tree) : 'a list =
	match tree with 
	| Empty -> []
	| Node (x, lhs, rhs) ->
		x :: preorder lhs @ preorder rhs


let inorder (tree : 'a binary_tree) : 'a list =
	match tree with 
	| Empty -> []
	| Node (x, lhs, rhs) ->
		preorder lhs @   x :: preorder rhs

(* TODO *)

(* 71. Dotstring representation of binary trees *)





let tests () = 

	assert (
		cbal_tree 4 =$=	
			[Node ('x', Node ('x', Empty, Empty),
			 Node ('x', Node ('x', Empty, Empty), Empty));
			 Node ('x', Node ('x', Empty, Empty),
			 Node ('x', Empty, Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
			 Node ('x', Empty, Empty));
			 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
			 Node ('x', Empty, Empty))]
	);


	assert (
		is_symmetric (
			Node (1, 
			      Node(2, Empty, Node(0,Empty,Empty)), 
			      Node(2, Node (0, Empty, Empty), Empty)
			)
		)
	);


	assert (
		is_symmetric (
			Node (1, 
			      Node(2, Empty, Node(0,Empty,Empty)), 
			      Node(2, Node (1, Empty, Empty), Empty)
			 )
		)
	);


	assert (
		not (is_symmetric (
			Node (1, 
			      Node(2, Empty, Node(0,Empty,Empty)), 
			      Node(2, Empty, Node (0, Empty, Empty))
			)
		))
	);


	assert (
		is_symmetric (construct [5; 3; 18; 1; 4; 12; 21])
	);

	assert (
		not (is_symmetric (construct [3; 2; 5; 7; 4]))
	);


	assert (
		construct [3; 2; 5; 7; 1]
			= Node (3, Node (2, Node (1, Empty, Empty), Empty),
			   Node (5, Empty, Node (7, Empty, Empty)))
	);


	assert (
		sym_cbal_tree 5 
			=$= [Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
			     Node ('x', Empty, Node ('x', Empty, Empty)));
			     Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
			     Node ('x', Node ('x', Empty, Empty), Empty))]
	);

	assert (
		List.length (cbal_tree 11) = nb_bal_tree 11
	);

	assert (
		List.length (cbal_tree 32) = nb_bal_tree 32
	);

	assert (
		List.length (cbal_tree 24) = nb_bal_tree 25
	);


	assert (
		List.length (sym_cbal_tree 57) = 256
	);

	assert (
		List.length (sym_cbal_tree 12) = 0
	);

	assert (
		List.length (sym_cbal_tree 13) = nb_sym_bal_tree 13
	);

	assert (
		List.length (sym_cbal_tree 21) = nb_sym_bal_tree 21
	);

	assert (
		hbal_tree 3 =$= 
			[Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
			 Node ('x', Empty, Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
			 Node ('x', Node ('x', Empty, Empty), Empty));
			 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
			 Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
			 Node ('x', Empty, Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
			 Node ('x', Node ('x', Empty, Empty), Empty));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
			 Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
			 Node ('x', Empty, Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
			 Node ('x', Node ('x', Empty, Empty), Empty));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
			 Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
			 Node ('x', Empty, Empty));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
			 Node ('x', Empty, Empty));
			 Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
			 Node ('x', Empty, Empty));
			 Node ('x', Node ('x', Empty, Empty),
			 Node ('x', Empty, Node ('x', Empty, Empty)));
			 Node ('x', Node ('x', Empty, Empty),
			 Node ('x', Node ('x', Empty, Empty), Empty));
			 Node ('x', Node ('x', Empty, Empty),
			 Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
	);


	assert (
		leaves Empty = []
	);

	assert (
		leaves (Node ('a', 
		              Node ('c', Empty, Empty), 
		              Node('b', Empty, Empty)
		       ))
			= ['c'; 'b']
	);


	assert (
		count_leaves Empty = 0
	);

	assert (
		count_leaves (Node ('a', 
		              Node ('c', Empty, Empty), 
		              Node('b', Empty, Empty)
		       ))
			= 2
	);

	assert (
		at_level (Node ('a', Node ('c', Empty, Empty), Node('b', Empty, Empty))) 3
			= []
	);


	assert (
		at_level (Node ('a', Node ('c', Empty, Empty), Node('b', Empty, Empty))) 2
			= ['c'; 'b']
	);

	assert (
		at_level (Node ('a', Node ('c', Empty, Empty), Node('b', Empty, Empty))) 1
			= ['a']
	);

	assert (
		at_level (Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)), Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty))))
			2 = ['b'; 'c']
	);

	assert (
		internals Empty = []
	);

	assert (
		internals (Node ('a', 
		              Node ('c', Empty, Empty), 
		              Node('b', Empty, Empty)
		       ))
			= ['a']
	);

	assert (
		internals (Node ('a', Empty, Empty))
			= []
	);

