true  :- =(a,a).
false :- =(a,b).

\+(Goal) :- Goal, !, false.
\+(Goal).

\=(X, Y) :- \+( =(X, Y) ).

repeat.
repeat :- repeat.

reverse(Xs, Res) :- reverse(Xs, [], Res).
reverse([], Acc, Acc).
reverse(.(X,Xs), Acc, Res) :- reverse(Xs, .(X,Acc), Res).
