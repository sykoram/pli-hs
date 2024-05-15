world.

world2().

true :- =(a,a).

true2() :- =(a,a).

world3 :- true.

man(adam).

parent(adam, kain).

mankind :- man(_).

p(q(X),Yyy) :-
    \+(=(X, Yyy)),
    r(Y)
    .
