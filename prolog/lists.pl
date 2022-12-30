
% 1.01 Find the last element of a list.
my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).


% 1.02 Find the last but one element of a list.
my_last2(X, [X, _]).
my_last2(X, [_|T]) :- my_last2(X, T).


% 1.03 Find the K'th element of a list.
element_at(X, [X|_], 1).
element_at(X, [_|T], N) :- 
    N > 1, M is N - 1,
    element_at(X, T, M).


% 1.04 Find the number of elements of a list.
len([], 0).
len([_|T], N) :- 
    N is M+1,
    len(T, M).


% 1.05 Reverse a list.
snoc(X, [], [X]).
snoc(X, [Y|Ys], [Y|Zs]) :- snoc(X, Ys, Zs).

rev([], []).
rev([X|Xs], Ys) :- 
    snoc(X, Zs, Ys),
    rev(Xs, Zs).


% 1.06 Find out whether a list is a palindrome.
palin(X) :- rev(X, X).     


% 1.07 Flatten a nested list structure.
my_flatten(X, [X]) :- \+ is_list(X).
my_flatten([], []).
my_flatten([H|T], L) :-
    my_flatten(H, H2),
    my_flatten(T, L2),
    append(H2, L2, L).


% 1.08 Eliminate consecutive duplicates of list elements.
compress([], []).
compress([X,X|T1], [X|T2]) :- compress(T1, T2), !.
compress([ X |T1], [X|T2]) :- compress(T1, T2).


% 1.09 Pack consecutive duplicates of list elements into sublists.
pack([], []).
pack([X|Xs], [Result|Ys]) :- 
    pack_first([X|Xs], Result, Rest),
    pack(Rest, Ys).

pack_first(X, [],  [X],  []).
pack_first(X, [Y|Xs], [X], [Y|Xs]) :- X \= Y.
pack_first(X, [X|Xs], [X|Result], Rest) :- 
    pack_first(X, Xs, Result, Rest).


% 1.10 Run-length encoding of a list.
rle_encode([], []).
rle_encode([X|Xs], [rle(X,Len)|Rest]) :- 
    pack_first(X, Xs, Ys, Rest1),
    length(Ys, Len),
    rle_encode(Rest1, Rest).


% 1.11 Modified run-length encoding.
rle_encode2([], []).
rle_encode2([X|Xs], [X|Rest]) :- 
    pack_first(X, Xs, [X], Rest1),
    rle_encode2(Rest1, Rest).

rle_encode2([X|Xs], [rle(X,Len)|Rest]) :- 
    pack_first(X, Xs, Ys, Rest1),
    length(Ys, Len),
    Len \= 1,
    rle_encode2(Rest1, Rest).


% 1.12 Decode a run-length encoded list.
rle_decode([], []).

rle_decode([rle(X, N)|RLEs], Result) :-
    rle_decode(RLEs, T),
    appendn(X, N, T, Result).

rle_decode([X|RLEs], [X|T]) :-
    X \= rle(_, _),
    rle_decode(RLEs, T).

appendn(_, 0, L, L).
appendn(X, N, L1, L2) :- 
    N > 0,
    M is N-1,
    appendn(X, M, [X|L1], L2).


% 1.13 Run-length encoding of a list (direct solution).
rle_encode_direct([], []).

% nth repeat
rle_encode_direct([X|Xs], [rle(X,N)|Ys]) :-
    rle_encode_direct(Xs, [rle(X,M)|Ys]),
    N is M+1,
    !.

% First repeat
rle_encode_direct([X|Xs], [rle(X,2)|Ys]) :-
    rle_encode_direct(Xs, [X|Ys]), !.

% Not a repeat
rle_encode_direct([X|Xs], [X|Ys]) :-
    rle_encode_direct(Xs, Ys).


% 1.14 Duplicate the elements of a list. 
dupli([], []).
dupli([X|Xs], [X,X|Ys]) :- dupli(Xs, Ys).



% 1.15 Duplicate the elements of a list a given number of times.
% TODO


% 1.16 Drop every N'th element from a list.
drop([], _, []).
drop(L1, N, L2) :- drop(L1, N, N, L2).


drop([], _, _, []).
drop([X|Xs], N, I, [X|Ys]) :-
    I > 1,
    J is I-1,
    drop(Xs, N, J, Ys).

drop([_|Xs], N, 1, Ys) :- 
    drop(Xs, N, N, Ys). 


% 1.17 Split a list into two parts; the length of the first part is given.
split([], _, [], []).
split(Xs, 0, [], Xs).
split([X|Xs], N, [X|Ys], Zs) :-
    N > 0,
    M is N-1,
    split(Xs, M, Ys, Zs).


% 1.18 Extract a slice from a list.
% TODO


% 1.19 Rotate a list N places to the left.
rotate([], _, []).
rotate(Xs, 0, Xs).
rotate([X|Xs], N, Ys) :- 
    N > 0,
    M is N-1,
    snoc(X, Xs, Zs),
    rotate(Zs, M, Ys).


% 1.20 Remove the K'th element from a list.
remove_at(X, [X|Xs], 1, Xs).
remove_at(Y, [X|Xs], N, [X|Ys]) :-
    N > 1,
    M is N-1, 
    remove_at(Y, Xs, M, Ys).


% 1.21 Insert an element at a given position into a list.
insert_at(X, Xs, 1, [X|Xs]).
insert_at(X, [Y|Xs], N, [Y|Ys]) :-
    N > 0,
    M is N-1,
    insert_at(X, Xs, M, Ys).


% 1.22 Create a list containing all integers within a given range.
range(X, X, [X]).
range(X, Y, [X|Xs]) :-
    X < Y,
    Z is X+1,
    range(Z, Y, Xs).


% 1.23 Extract a given number of randomly selected elements from a list.
rnd_select(_, 0, []).
rnd_select(Xs, N, [Y|Ys]) :- 
    N > 0,
    length(Xs, Len),
    Max is Len+1,
    random(1, Max, R),
    remove_at(Y, Xs, R, Zs),
    M is N-1,
    rnd_select(Zs, M, Ys).


% 1.24 Lotto: Draw N different random numbers from the set 1..M.
lotto(N, Max, L) :- 
    range(1, Max, R),
    rnd_select(R, N, L).
 

% 1.25 Generate a random permutation of the elements of a list.
rnd_permu(Xs, Ys) :-
    length(Xs, Len),
    rnd_select(Xs, Len, Ys).


% 1.26 Generate the combinations of K distinct objects chosen from the N elements of a list
combination(0, _, []).
combination(N, [X|Xs], [X|Ys]) :-
    N > 0,
    M is N-1,
    combination(M, Xs, Ys).

combination(N, [_|Xs], Ys) :-
    N > 0,
    combination(N, Xs, Ys).


% 1.27 Group the elements of a set into disjoint subsets.

% a) E: 2people, F: 3p, G: 4p
group3([], [], [], []).
group3(Xs, E, F, G) :- fail. 




% 1.28 Sorting a list of lists according to length of sublists

% a)

% b)



