true  :- =(a,a).
false :- =(a,b).

\+(Goal) :- Goal, !, false.
\+(Goal).

\=(X, Y) :- \+( =(X, Y) ).

repeat.
repeat :- repeat.

length([], 0).
length(.(_,Xs), s(L)) :- length(Xs, L).

reverse(Xs, Res) :- reverse(Xs, [], Res).
reverse([], Acc, Acc).
reverse(.(X,Xs), Acc, Res) :- reverse(Xs, .(X,Acc), Res).
