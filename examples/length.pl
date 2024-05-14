length([], 0).
length(.(_,Xs), s(L)) :- length(Xs, L).
