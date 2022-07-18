
(* 31. Determine whether a given integer number is prime *)
let is_prime (n : int) : bool =
	let rec aux i = 
		if i*i > n then true
		else if n mod i = 0 then false
		else aux (i+1) 

	in n <> 1 && aux 2


(* 32. Determine the greatest common divisor of two positive integer numbers *)
let rec gcd (a : int) (b : int) : int = 
	if a = b then a
	else if a > b then gcd (a-b) b
	else gcd (b-a) a


(* 33. Determine whether two positive integer numbers are coprime *)
let coprime (a : int) (b : int) : bool = 
	gcd a b = 1


(* 34. Calculate Euler's totient function φ(m) *)
let phi (n : int) : int =
	let rec aux acc i = 
		if i >= n then acc
		else if coprime i n then aux (acc+1) (i+1)
		else aux acc (i+1)
	in aux 0 1


(* 35. Determine the prime factors of a given positive integer *)
let factors (n : int) : int list = 

	let rec aux acc i n = 
		if i > n then acc

		else if n mod i = 0 
		then aux (i::acc) i (n / i)
		
		else aux acc (i+1) n

	in List.rev (aux [] 2 n)


(* 36. Determine the prime factors of a given positive integer (2) *)
let factors' (n : int) : (int * int) list = 

	let rec rle = function
		| []   -> []
		| p::t -> 
		match rle t with
			| [] -> [(p, 1)]
			| (q, n)::t when p = q -> (q, n+1)::t
			| (q, n)::t -> (p, 1)::(q, n)::t

	in factors n |> rle


(* 37. Calculate Euler's totient function φ(m) (improved) *)
let rec pow x n =
	if n <= 0 then 1 else
	let p = pow x (n/2) in 
	if n mod 2 = 0 then p*p	else p*p*x


let phi_improved (n : int) : int =
	factors' n |> List.fold_left (
		fun acc (p,m) -> acc * (p-1) * (pow p (m-1))
	) 1


(* 38. Compare the two methods of calculating Euler's totient function *)
let time_phi () = 

	let timeit (f : unit -> 'b) : float * 'b = 
		let now = Unix.gettimeofday() in
		let res = f () in
		Unix.gettimeofday() -. now , res
	in	

	let t1, _ = timeit (fun () -> phi 10090) in
	let t2, _ = timeit (fun () -> phi_improved 10090) in

	print_string "phi_improved is ";
	print_float (t1 /. t2);
	print_string " times faster than phi\n"


(* 39. A list of prime numbers *)
let all_primes (max : int) : int list =

	(* sieve of eratosthenes  *)
	let rec sieve acc = function
		| []   -> acc
		| h::t -> sieve (h::acc) 
			(List.filter (fun x -> x mod h <> 0) t)
	in 

	(* range 2..max *)
	List.init (max-2) (fun i -> i+2)
		|> sieve [] 
		|> List.rev 


let all_primes' (min : int) (max : int) : int list =
	all_primes max |> List.filter (fun x -> x >= min)


(* 40. Goldbach's conjecture *)
let goldbach (n : int) = 
	let primes = all_primes n in
	primes |> List.map (fun p -> (p, n - p) )
		   |> List.find (fun (p,q) -> List.mem q primes)


(* gives all the solutions *)
let goldbach' (n : int) = 
	let primes = all_primes n in
	primes |> List.map (fun p -> (p, n - p) )
		   |> List.filter (fun (p,q) -> List.mem q primes)


(* 41. A list of Goldbach compositions *)
let goldbach_list (min:int) (max:int) : (int*(int*int)) list =
	List.init (max-min+1) (fun x -> x+min)
	|> List.filter (fun x -> x > 2 && x mod 2 = 0)
	|> List.map (fun x -> x, goldbach x)


(* gives all the solutions for each number *)
let goldbach_list' (max : int) : (int*(int*int)) list =
	let rec cartesian = function
		| [], _ | _, [] -> []
		| x::xs, y::ys  -> 
			[(x+y, (x, y))] 
			@ cartesian (xs, ys)
			@ cartesian (x::xs, ys)
			@ cartesian (xs, y::ys)
	in

	let primes = all_primes max in
	cartesian (primes, primes)


(* bonus exercice : finding p,q couples where p and q are big *)
let goldbach_big (min:int) (max:int) (lim:int) : (int*(int*int)) list =
	goldbach_list min max |> List.filter 
		(fun (_,(p,q)) -> p >= lim && q >= lim)