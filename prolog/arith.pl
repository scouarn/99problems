% 2.01 (**) Determine whether a given integer number is prime.

is_prime(N, I) :-
    I * I > N, !.

is_prime(N, I) :-
    N mod I =\= 0,
    J is I + 1,
    is_prime(N, J).

is_prime(N) :-
    N >= 2,
    is_prime(N, 2).


% 2.02 (**) Determine the prime factors of a given positive integer.

primes_upto(N, []) :-
    N < 2, !.

primes_upto(N, [N|Ps]) :-
    is_prime(N), !,
    M is N-1,
    primes_upto(M, Ps).

primes_upto(N, Ps) :-
    M is N-1,
    primes_upto(M, Ps).


prime_factors(_, [], []).

% Not a divisor
prime_factors(N, [P|Ps], Fs) :-
    N mod P =\= 0,
    prime_factors(N, Ps, Fs),
    !.

% Divisor found
prime_factors(N, [P|Ps], [P|Fs]) :-
    M is N // P,
    prime_factors(M, [P|Ps], Fs).


prime_factors(N, Fs) :-
    primes_upto(N, Ps),
    prime_factors(N, Ps, Fs).


% 2.03 (**) Determine the prime factors of a given positive integer (2).

collapse([], []).

collapse([X|Xs], [f(X,N)|Ys]) :-
    collapse(Xs, [f(X,M)|Ys]),
    N is M+1,
    !.

collapse([X|Xs], [f(X,1)|Ys]) :-
    collapse(Xs, Ys).

prime_factors_mult(N, Fs) :-
    prime_factors(N, Xs),
    collapse(Xs, Fs).


% 2.04 (*) A list of prime numbers.
% (in a range)

prime_range(Lo, Up, []) :-
    Lo > Up, !.

prime_range(Lo, Up, [Lo|Ps]) :-
    is_prime(Lo), !,
    Lo1 is Lo + 1,
    prime_range(Lo1, Up, Ps).

prime_range(Lo, Up, Ps) :-
    Lo1 is Lo + 1,
    prime_range(Lo1, Up, Ps).


