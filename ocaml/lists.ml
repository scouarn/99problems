(* 1. Tail of a list *)
let rec last (lst : 'a list) : 'a option =
	match lst with
	| []   -> None
	| [h]  -> Some h
	| _::t -> last t


(* 2. Last two elements of a list *)
let rec last_two (lst : 'a list) : ('a * 'a) option = 
	match lst with
	| [] | [_] -> None
	| [p;q]    -> Some (p, q) 
	| _::t     -> last_two t


(* 3. N'th element of a list *)
let rec nth (lst : 'a list) (n : int) : 'a option = 
	match lst with 
	| h::_ when n = 0 -> Some h
	| _::t -> nth t (n-1)
	| _ -> None


(* 4. Length of a list 
   Bonus for a tail recursive solution.
 *)
let length (lst : 'a list) : int = 
	let rec aux n = function 
		| []   -> n
		| _::t -> aux (n+1) t
	
	in aux 0 lst


(* 5. Reverse a list *)
let rec rev (lst : 'a list) : 'a list =
	match lst with
	| []   -> []
	| h::t -> rev t @ [h]

(* Solution with no @ *)
let rev' (lst : 'a list) : 'a list = 
	let rec aux acc = function
		| []   -> acc
		| h::t -> aux (h::acc) t
	in aux [] lst

(* Cute solution *)
let rev'' (lst : 'a list) : 'a list = 
	List.fold_left (fun acc x -> x::acc) [] lst 


(* 6. Palindrome *)
let rec is_palindrome (lst : 'a list) : bool = 
	rev lst = lst


(* 7. Flatten a list *)
type 'a node =
	| One  of 'a 
	| Many of 'a node list

let rec flatten (lst : 'a node list) : 'a list =
	match lst with 
	| [] -> []
	| One  h :: t -> h :: flatten t
	| Many h :: t -> flatten h  @  flatten t


(* 8. Eliminate duplicates *)
let rec compress (lst : 'a list) : 'a list = 
	match lst with 
	| []  -> []
	| [p] -> [p]
	| p::q::t when p = q -> compress (q::t)
	| p::q::t -> p :: compress (q::t)


(* 9. Pack consecutive duplicates *)
let pack (lst : 'a list) : 'a list list = 
	let rec aux acc = function
		| []  -> [acc]
		| [p] -> [p::acc]
		| p::q::t when p = q -> aux (q::acc) (q::t)
		| p::q::t -> (p::acc) :: aux [] (q::t)

	in aux [] lst


(* 10. Run-length encoding *)
let encode (lst : 'a list) : (int * 'a) list = 
	let rec aux n = function
		| []  -> []
		| [p] -> [(n, p)]
		| p::q::t when p = q -> aux (n+1) (q::t) 
		| p::q::t -> (n, p) :: aux 1 (q::t)

	in aux 1 lst

let rec encode' (lst : 'a list) : (int * 'a) list = 
	match lst with
		| []   -> []
		| p::u -> 
	match encode' u with
		| [] -> [(1, p)]
		| (n, q)::v when p = q -> (n+1, q)::v
		| (n, q)::v -> (1, p)::(n, q)::v


(* 11. Modified run-length encoding *)
type 'a rle =
	| One  of 'a
	| Many of int * 'a

let rle_cons : (int * 'a) -> 'a rle = function 
	| 1, p -> One p
	| n, p -> Many (n, p)

let encode'' (lst : 'a list) : 'a rle list = 
	lst |> encode |> List.map rle_cons


(* 12. Decode a run-length encoded list *)
let rec decode (lst : 'a rle list) : 'a list =
	match lst with
	| [] -> []
	| One  h :: t -> h :: decode t
	| Many (n, h) :: t -> (List.init n (fun _ -> h)) @ decode t


(* 13. Run-length encoding of a list (direct solution) *)
let encode''' (lst : 'a list) : 'a rle list = 
	let rec aux n = function
		| []  -> []
		| [p] -> [rle_cons (n, p)]
		| p::q::t when p = q -> aux (n+1) (q::t) 
		| p::q::t -> rle_cons (n, p) :: aux 1 (q::t)

	in aux 1 lst


(* 14. Duplicate the elements of a list *)
let rec duplicate (lst : 'a list) : 'a list =
	match lst with
	| [] -> []
	| h::t -> h :: h :: duplicate t


(* 15. Replicate the elements of a list a given number of times *)
let replicate (lst : 'a list) (n : int) : 'a list = 
	lst |> List.concat_map (
		fun x -> List.init n (fun _ -> x)
	)


(* 16. Drop every N'th element from a list *)
let drop (lst : 'a list) (n : int) : 'a list =
	let rec aux = function 
		| []  , _ -> []
		| h::t, 1 -> aux (t, n)
		| h::t, m -> h :: aux (t, m-1)

	in aux (lst, n)


(* 17. Split a list into two parts; the length of the first part is given *)
let split (lst : 'a list) (n : int) : 'a list * 'a list = 
	let rec aux n l1 = function
		| [] -> l1, []
		| l2 when n = 0 -> l1, l2
		| h::t -> aux (n-1) (l1 @ [h]) t

	in aux n [] lst


(* 18. Extract a slice from a list *)
let rec cut_nfirsts (n : int) : 'a list -> 'a list = 
	function
	| [] -> []
	| lst when n = 0 -> lst
	| h::t -> cut_nfirsts (n-1) t

let rec keep_nfirsts (n : int) : 'a list -> 'a list = 
	function 
	| [] -> []
	| _  when n = 0 -> []
	| h::t -> h :: keep_nfirsts (n-1) t

let slice (lst : 'a list) (i : int) (k : int) = 
	lst |> cut_nfirsts i |> keep_nfirsts (k-i+1)


(* 19. Rotate a list N places to the left *)
let rec rotate (lst : 'a list) (n : int) : 'a list = 
	match lst with
	| []  -> []
	| [h] -> [h]
	| lst when n = 0 -> lst
	| h::t -> rotate (t @ [h]) (n-1)


(* 20. Remove the K'th element from a list *)
let rec remove_at (n : int) (lst : 'a list) : 'a list = 
	match lst with 
	| [] -> []
	| _::t when n = 0 -> t
	| h::t -> h :: remove_at (n-1) t


(* 21. Insert an element at a given position into a list *)
let rec insert_at (x : 'a) (n : int) (lst : 'a list) : 'a list = 
	match lst with
	| [] -> [x]
	| lst when n = 0 -> x :: lst
	| h::t -> h :: insert_at x (n-1) t  


(* 22. Create a list containing all integers within a given range *)
let rec range (i : int) (k : int) : int list = 
	     if i < k then i :: range (i+1) k
	else if i > k then i :: range (i-1) k
	else [i]


(* 23. Extract a given number of randomly selected elements from a list *)
let rec rand_select (lst : 'a list) (n : int) : 'a list = 
	if n <= 0 then [] 
	else let idx = Random.int (length lst) in
		List.nth lst idx ::
		rand_select (remove_at idx lst) (n-1)
		

(* 24. Lotto: Draw N different random numbers from the set 1..M *)
let lotto_select (n : int) (m : int) : int list = 
	rand_select (range 1 m) n


(* 25. Generate a random permutation of the elements of a list *)
let permutation (lst : 'a list) : 'a list = 
	rand_select lst (length lst)


(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list *)
let rec prefix (x : 'a) (lst : 'a list list) : 'a list list =
	match lst with
	| []   -> []
	| h::t -> (x::h) :: prefix x t


(* Take all the subsets but only keep those with a length of n... *)
let extract (n : int) (lst : 'a list) = 

	let rec power_set = function
		| []   -> [[]]
		| h::t -> prefix h (power_set t) @ power_set t
	
	in power_set lst |> List.filter (fun l -> length l = n)


(* better version that does not compute every subsets *)
let rec extract' (n : int) (lst : 'a list) = 
	match lst with
	| _ when n = 0 -> [[]]
	| [] -> []
	| h::t -> 
		  prefix h (extract' (n-1) t) (* subsets with h *)
		@ extract' n t                (* subsets without h *)


(* 27. Group the elements of a set into disjoint subsets *)
(* extract from the previous remain and add to the list of extracted subsets *)
type 'a extr = 'a list list * 'a list
let rec extract_remain (n : int) (acc,lst : 'a extr) : 'a extr list = 
	match lst with
		| _ when n = 0 -> [[]::acc, lst]
		| [] -> []
		| h::t -> 
			(* subsets with h *)
			let with_h = extract_remain (n-1) (acc, t) 
				|> List.rev_map (fun (ext,rem) -> match ext with 
					| [] -> [[h]], rem
					| u::v -> (h::u)::v, rem
				)

			(* subsets without h *)
			and without_h = extract_remain n (acc, t)
				|> List.rev_map (fun (ext,rem) -> ext, h::rem)

			in with_h @ without_h



let group (lst : 'a list) (sizes : int list) : 'a list list list =
	let rec aux (ext, rem) = function 
		| [] -> [List.rev ext]
		| size::sizes -> 

		extract_remain size (ext, rem)
		|> List.concat_map (fun (ext',rem') -> 
				aux (ext', rem') sizes
			)

	in aux ([], lst) sizes



(* This is a hard problem and and I can't bother having the results in 
	same order as the example, so to be shure I tested my implementation against 
	the solution using multiset equality
 *)
let multiset_eq (xs : 'a list) (ys : 'a list) : bool = 
	let count n = List.fold_left (fun acc x -> acc + if n = x then 1 else 0) 0 in
	xs |> List.for_all (fun x -> count x xs = count x ys) &&
	ys |> List.for_all (fun y -> count y ys = count y xs)

let (=$=) = multiset_eq


(* The solution from ocaml.org *)
let group_solution lst sizes =
	let initial = List.map (fun size -> size, []) sizes in
	let prepend p lst =
		let emit l acc = l :: acc in
		let rec aux emit acc = function
			| [] -> emit [] acc
			| (n, l) as h :: t ->
				let acc = if n > 0 then emit ((n - 1, p :: l) :: t) acc
				          else acc in
					aux (fun l acc -> emit (h :: l) acc) acc t
		in aux emit [] lst

	in let rec aux = function
		| [] -> [initial]
		| h :: t -> List.concat (List.map (prepend h) (aux t))
	
	in	let all = aux lst in
	let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
	List.map (List.map snd) complete;;



(* 28. Sorting a list of lists according to length of sublists *)
let length_sort (lst : 'a list list) : 'a list list =
	lst |> List.sort (
		fun l1 l2 -> List.compare_lengths l1 l2 
		(* compare (length l1) (length l2) *)
	)

let frequency_sort (lst : 'a list list) : 'a list list = 
	let sublst_lengths = lst |> List.map (fun l -> length l) in

	let freq l = (* count occurences of length(l) *)
		sublst_lengths |> List.filter (fun x -> x = length l) |> length
	in 

	lst |> List.sort (fun l1 l2 -> compare (freq l1) (freq l2)) 




let tests () = 

	assert (
		last ["a" ; "b" ; "c" ; "d"] = Some "d"
	);

	assert (
		last [] = None
	);

	assert (
		last_two ["a"; "b"; "c"; "d"] = Some ("c", "d")
	);

	assert (
		last_two ["a"] = None
	);

	assert (
		nth ["a"; "b"; "c"; "d"; "e"] 2 = Some "c"
	);

	assert (
		nth ["a"] 2 = None
	);
	
	assert (
		length ["a"; "b"; "c"] = 3
	);
	
	assert (
		length [] = 0
	);
	
	assert (
		rev ["a"; "b"; "c"] = ["c"; "b"; "a"]
	);

	assert (
		rev' ["a"; "b"; "c"] = ["c"; "b"; "a"]
	);

	assert (
		rev'' ["a"; "b"; "c"] = ["c"; "b"; "a"]
	);

	assert (
		is_palindrome ["x"; "a"; "m"; "a"; "x"] = true
	);

	assert (
		not (is_palindrome ["a"; "b"]) = true
	);

	assert (
		flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
			= ["a"; "b"; "c"; "d"; "e"]
	);

	assert (
		compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
			= ["a"; "b"; "c"; "a"; "d"; "e"]
	);

	assert (
		pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
			= [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
	);

	assert (
		encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
			= [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
	);

	assert (
		encode' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
			= [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
	);

	assert (
		encode'' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
			= [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
	);

	assert (
		decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
			= ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
	);

	assert (
		encode''' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
			= [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
	);

	assert (
		encode''' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
			= [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
	);

	assert (
		duplicate ["a"; "b"; "c"; "c"; "d"]
			= ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
	);

	assert (
		replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
	);

	assert (
		drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
			= ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
	);

	assert (
		split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
			= (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
	);

	assert (
		split ["a"; "b"; "c"; "d"] 5
			= (["a"; "b"; "c"; "d"], [])
	);

	assert (
		slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6
			= ["c"; "d"; "e"; "f"; "g"]
	);

	assert (
		rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
		= ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
	);

	assert (
		remove_at 1 ["a"; "b"; "c"; "d"]
			= ["a"; "c"; "d"]
	);

	assert (
		insert_at "alfa" 1 ["a"; "b"; "c"; "d"]
			= ["a"; "alfa"; "b"; "c"; "d"]
	);

	assert (
		range 4 9 = [4; 5; 6; 7; 8; 9]
	);

	Random.init 0;
	assert (
		rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
			= ["a"; "h"; "d"]
	);

	Random.init 0;
	assert (
		lotto_select 6 49	= [14; 32; 16; 46; 41; 4]
	);

	Random.init 0;
	assert (
		permutation ["a"; "b"; "c"; "d"; "e"; "f"]
			= ["c"; "a"; "e"; "d"; "b"; "f"]
	);

	assert (
		extract 2 ["a"; "b"; "c"; "d"]
			=$= [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
	);

	assert (
		extract' 2 ["a"; "b"; "c"; "d"]
			=$= [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
	);

	assert (
		group ["a"; "b"; "c"; "d"] [2; 1]
			=$= [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
			     [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
			     [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
			     [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
	);

	assert (
		group_solution ["a"; "b"; "c"; "d"] [2; 1]
			=$= group   ["a"; "b"; "c"; "d"] [2; 1]
	);

	assert (
		group_solution ["a"; "b"; "c"; "d"; "e"] [3; 1]
			=$= group   ["a"; "b"; "c"; "d"; "e"] [3; 1]
	);

	assert (
		group_solution ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] [6; 2]
			=$= group   ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] [6; 2]
	);

	assert (
		length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]
			= [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["i"; "j"; "k"; "l"]]
	);

	assert (
		frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]
			= [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]]
	);
