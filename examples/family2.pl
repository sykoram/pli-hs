parent(adam, kain).
parent(adam, abel).
parent(eva, kain).
parent(eva, abel).

have_multiple_children(X, Y) :-
    parent(X, C1),
    parent(Y, C1),
    parent(X, C2),
    parent(Y, C2),
    \=(C1, C2).
