`pli` is demo Prolog intepreter written in Haskell.

- Both facts `fact(a).` and rules `rule(X) :- subgoal1(X), subgoal2(X).` are supported.
- All predicates must be written in form `functor(...)` - even the unification: `=(X, p(a))`.
- Cuts `!` are supported.
- Lists are written as `.(a, .(b, .(c, [])))` (element `a`, `b` and `c`).
- Natural numbers are written as `s(s(0))` (this is 2).
- Unknown predicates are treated as false (beware of typos!).

To run the program ([cabal](https://www.haskell.org/cabal/) must be installed):
```
cabal run pli -- INPUT_FILE
```
where `INPUT_FILE` is the path to the prolog program file.

Examples to try:
- `examples/lib.pl` (contains few essential predicates)
  - `=(Xs, .(a,.(b,.(c,.(d,[])))) ), length(Xs, Length).` (get length of the list `[a,b,c,d]`)
  - `=(Xs, .(a,.(b,.(c,.(d,[])))) ), reverse(Xs, Rev).` (reverse the list `[a,b,c,d]`)
  - `length(Xs, s(s(0))).` (generate a list of length 2)
  - `length(Xs, Len).` (`[Enter]` or `;`+`[Enter]` to generate next result; `.`+`[Enter]` to stop)
  - `length(Xs, Len), =(Len, s(s(s(_))) ), =(Xs, .(first, _) ).` (list of at least 3 elements, the first of which is `first`)
  - `reverse(Xs, Rev), =(Xs, .(first,_) ), =(Rev, .(last,_) ).` (reverse of a list that starts with `first` and ends with `last`)
- `examples/rle.pl` ([Run Length Encoding](https://en.wikipedia.org/wiki/Run-length_encoding))
  - `rle( .(a,.(a,.(a,.(b,.(b,[]))))) , RLE).` (encode `[a,a,a,b,b]`)
