\+(Goal) :- Goal, !, false.
\+(Goal).

\=(X, Y) :- \+( =(X, Y) ).

reverse(Xs, Res) :- reverse(Xs, [], Res).
reverse([], Acc, Acc).
reverse(.(X,Xs), Acc, Res) :- reverse(Xs, .(X,Acc), Res).


rle(X, RLE) :- rle(X, [], RLE).

rle([], A, RLE) :-
    reverse(A, RLE).
rle(.(X,Xs), [], RLE) :-
    rle(Xs, .(pair(s(0),X), []), RLE).
rle(.(X,Xs), .(pair(N,X), A), RLE) :-
    rle(Xs, .(pair(s(N),X), A), RLE).
rle(.(X,Xs), .(pair(N,Y), A), RLE) :-
    \=(X, Y),
    rle(Xs, .(pair(s(0),X), .(pair(N,Y), A)), RLE).
