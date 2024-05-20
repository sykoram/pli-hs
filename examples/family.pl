true  :- =(a,a).
false :- =(a,b).

\+(Goal) :- Goal, !, false.
\+(Goal).

\=(X, Y) :- \+( =(X, Y) ).

man(adam).
man(kain).
man(abel).

woman(eva).

parent(adam, kain).
parent(adam, abel).
parent(eva, kain).
parent(eva, abel).

mother(Mother, Child) :-
    woman(Mother),
    parent(Mother, Child).

brother(B, X) :-
    man(B),
    parent(P, B),
    parent(P, X),
    \=(B, X).

have_multiple_children(X, Y) :-
    parent(X, C1),
    parent(Y, C1),
    \=(X, Y),
    parent(X, C2),
    parent(Y, C2),
    \=(C1, C2).
