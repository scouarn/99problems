

% 4.01 Check whether a given term represents a binary tree
istree(nil).
istree(t(_, A, B)) :- istree(A), istree(B).


% 4.02 Construct completely balanced binary trees


