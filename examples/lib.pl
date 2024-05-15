true  :- =(a,a).
false :- =(a,b).

\=(X, Y) :- \+( =(X, Y) ).

reverse(Xs, Res) :- reverse(Xs, [], Res).
reverse([], Acc, Acc).
reverse(.(X,Xs), Acc, Res) :- reverse(Xs, .(X,Acc), Res).
