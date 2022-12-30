
% 3.01 (**) Truth tables for logical expressions.
and(A, B) :- A, B.

or(A, _) :- A, !.
or(_, B) :- B.

nand(A, B) :- \+ and(A, B).
nor(A, B)  :- \+ or(A, B).

xor(A, B) :- A, !, \+ B.
xor(A, B) :- B, \+ A.

impl(A, B) :- or(\+ A, B).

equ(A, B) :- A, !, B.
equ(A, B) :- \+ A, \+ B.


bind(true).
bind(fail).

write_res(Expr) :- Expr, !, write(true).
write_res(Expr) :- write(fail).

table(A, B, Expr) :-
    bind(A), bind(B),
    write(A), write(" "),
    write(B), write(" "),
    write_res(Expr), nl,
    fail. % don't write the results

