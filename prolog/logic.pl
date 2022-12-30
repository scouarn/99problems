
% 3.01 (**) Truth tables for logical expressions.

% not(A) :- \+ A.
not(A) :- A, !, fail.
not(_).

and(A, B) :- A, B.

or(A, _) :- A, !.
or(_, B) :- B.

nand(A, B) :- not(and(A, B)).
nor(A, B)  :- not(or(A, B)).

xor(A, B) :- A, !, not(B).
xor(A, B) :- B, not(A).

impl(A, _) :- not(A), !.
impl(_, B) :- B.

equ(A, B) :- A, !, B.
equ(A, B) :- not(A), not(B).



bind(true).
bind(fail).

write_res(Expr) :- Expr, !, write(true).
write_res(_)    :- write(fail).

table(A, B, Expr) :-
    bind(A), bind(B),
    write(A), write(' '),
    write(B), write(' '),
    write_res(Expr), nl,
    fail. % don't write the results


% 3.02 (*) Truth tables for logical expressions (2).
:- op(300, fx,  not).
:- op(400, xfy, [and, nand, nor, xor]).
:- op(500, xfy, or).
:- op(700, xfy, impl).
:- op(600, xfy, equ).


% 3.03 (**) Truth tables for logical expressions (3).
bind_vars([]).
bind_vars([X|Xs]) :-
    bind(X),
    bind_vars(Xs).

write_vars([]).
write_vars([X|Xs]) :-
    write(X), write(' '),
    write_vars(Xs).

table(Vars, Expr) :-
    bind_vars(Vars),
    write_vars(Vars),
    write_res(Expr), nl,
    fail. % don't write the results


% 3.04 (**) Gray code.

% Add C in front of every X in Xs
distrib(_, [], []).
distrib(C, [X|Xs], [Y|Ys]) :-
    atom_concat(C, X, Y),
    distrib(C, Xs, Ys).

% To compute gray(N+1) knowing gray(N) :
% C1: Take gray(N) and add '0's in front of the codes
% C2: Take gray(N), reverse the list and add '1's in front of the codes
% Concat C1 and C2.
gray(1, ['0', '1']) :- !.
gray(N, C) :-
    M is N-1,
    gray(M, C1),

    distrib('0', C1, Left),

    reverse(C1, Crev),
    distrib('1', Crev, Right),

    append(Left, Right, C).


% 3.05 (***) Huffman code.

% Insert in an ordered list
insert(F, [], [F]).
insert(F, [X|Xs], [F,X|Xs]) :-
    F = fr(_, F1),
    X = fr(_, F2),
    F1 =< F2,
    !.
insert(F, [X|Xs], [X|Ys]) :-
    insert(F, Xs, Ys).

% Sort the list of frequencies (only used once to preprocess)
build_list([], []).
build_list([X|Xs], Zs) :-
    build_list(Xs, Ys),
    insert(X, Ys, Zs).

% Take out of the list the two subtrees with the lowest frequency,
% Add the frequencies and put in a node,
% Insert back in order in the list,
% Stop when there is only the root left
build_tree([Tree], Tree).
build_tree([L,R|Fs], Tree) :-
    L = fr(_, F1),
    R = fr(_, F2),
    F3 is F1 + F2,
    insert(fr(node(L, R), F3), Fs, Fs2),
    build_tree(Fs2, Tree).


% Write fr(node(fr(a, 45), fr(b, 10)), 55) as
% Node(55) {
%     Leaf(a, 45)
%     Leaf(b, 10)
% }
indent(0) :- !.
indent(N) :-
    write('    '),
    M is N-1,
    indent(M).

% Leaf
write_tree(fr(Leaf, F), N) :-
    indent(N), write('Leaf('), write(Leaf), write(', '),
    write(F), write(')'), nl.

% Node
write_tree(fr(node(L, R), F), N) :-
    indent(N), write('Node('), write(F), write(') {'), nl,
    M is N+1,
    write_tree(L, M),
    write_tree(R, M),
    indent(N), write('}'), nl.

write_tree(Tree) :-
    write_tree(Tree, 0).


% Build the Huffman code :
% When going left add a '0' to the code
% When going right add a '1'
build_code(fr(Leaf, _), Code, [hc(Leaf,Code)]) :-
    Leaf \= node(_, _).

build_code(fr(node(L, R), _), Code, Hc) :-
    atom_concat(Code, '0', CodeL),
    atom_concat(Code, '1', CodeR),
    build_code(L, CodeL, HcL),
    build_code(R, CodeR, HcR),

    append(HcL, HcR, Hc).

% Sort the frequencies,
% Build the tree,
% Compute the code
huffman(Fs, Hc) :-
    build_list(Fs, Fs_sorted),
    build_tree(Fs_sorted, Tree),
    build_code(Tree, '', Hc).

