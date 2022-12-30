
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

