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
    parent(P, X).
