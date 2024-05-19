module Operations(unify, renameVars) where

import Data.List (foldl')
import qualified Data.Map as Map
import Common

{-|
Substitutes a value (Term) in for a variable (identified by VarId) in the given Term.

>>> substitute 2 (Atom "xyz") (Comp "p" [Var 2, Var 3, Comp "q" [Var 2]])
"p"("xyz", #3, "q"("xyz"))
-}
substitute :: VarId -> ITerm -> ITerm -> ITerm
substitute var val term = case term of
  (Atom atom)                -> Atom atom
  (Var other) | var == other -> val
              | otherwise    -> Var other
  (Comp ftor args)           -> Comp ftor (map (substitute var val) args)

{-|
Creates a binding for a new variable (substitution in existing bindings is also executed).

>>> bind 1 (Atom "a") (Map.fromList [(0,Comp "p" [Var 1])])
fromList [(0,"p"("a")),(1,"a")]
-}
bind :: VarId -> ITerm -> Bindings -> Bindings
bind var val bindings
  | var `Map.notMember` bindings = Map.insert var val (Map.map (substitute var val) bindings) -- replace the newly binded variable in the values with its value
  | otherwise                    = error "Binding a variable that has already been binded."

{-|
Applies all binding onto the given term.

>>> applyBindings (Map.fromList [(0,Atom "a")]) (Comp "p" [Var 0,Var 1])
"p"("a", #1)
-}
applyBindings :: Bindings -> ITerm -> ITerm
applyBindings bindings term = case term of
  (Atom a)      -> Atom a
  (Var v)       -> Map.findWithDefault (Var v) v bindings
  (Comp f args) -> Comp f (map (applyBindings bindings) args)

{-|
Unifies the two terms and updates the bindings. Returns Nothing on failure.

Example: unify `p(q(a),X0,q(X1))` and `p(X2,X3,X3)` --> `X0 = q(X1), X3 = q(X1), X2 = q(a)`

>>> let t1 = Comp "p" [Comp "q" [Atom "a"],Var 0,Comp "q" [Var 1]] ; t2 = Comp "p" [Var 2,Var 3,Var 3] in unify t1 t2 Map.empty
Just (fromList [(0,"q"(#1)),(2,"q"("a")),(3,"q"(#1))])

Example: unify `p(X0,X0,X0)` and `p(X1,q(X2),q(q(X3)))` --> `X0 = q(q(X3)), X1 = q(q(X3)), X2 = q(X3)`

>>> let t1 = Comp "p" [Var 0,Var 0,Var 0] ; t2 = Comp "p" [Var 1,Comp "q" [Var 2],Comp "q" [Comp "q" [Var 3]]] in unify t1 t2 Map.empty
Just (fromList [(0,"q"("q"(#3))),(1,"q"("q"(#3))),(2,"q"(#3))])
-}
unify :: ITerm -> ITerm -> Bindings -> Maybe Bindings
unify term1 term2 bindings = case (applyBindings bindings term1, applyBindings bindings term2) of -- apply bindings before unifying => binded variables shouldn't occur in the terms afterwards
  (Atom a, Atom b) | a == b    -> Just bindings
  (Var u,  Var v)  | u == v    -> Just bindings
                   | u < v     -> Just (bind u (Var v) bindings) -- order the variables (maybe not necessary)
                   | otherwise -> Just (bind v (Var u) bindings)
  (Var v,  term)   -> Just (bind v term bindings) -- no occurs check!
  (term,   Var v)  -> Just (bind v term bindings) -- no occurs check!
  (Comp f1 args1, Comp f2 args2) | sameComp -> foldl' unifyArgsStep (Just bindings) (zip args1 args2) -- unify arguments one after each other
    where
      sameComp = f1 == f2 && length args1 == length args2 -- the same functor and arity
      unifyArgsStep maybeBindings (a1, a2) = maybeBindings >>= unify a1 a2 -- calls unify if maybeBindings has a value (is Just)
  (_, _) -> Nothing -- cannot unify different Atoms, an Atom with a Comp or Comps with a different functor or arity

{-|
Increases ids of all variables so the least id is now equal to nextId.

>>> renameVars (Clause (Comp "bros" [Var 2,Var 3]) [Comp "man" [Var 2], Comp "man" [Var 3], Comp "\\=" [Var 2,Var 3]]) 10
(Clause "bros"($10, $11) ["man"($10),"man"($11),"\\="($10, $11)],12)
-}
renameVars :: Clause -> VarId -> (Clause, VarId)
renameVars clause@(Clause cHead cBody) nextId = case collectIds cHead ++ concatMap collectIds cBody of
  []  -> (clause, nextId) -- no id => return the original clause
  ids -> let by = nextId - minimum ids -- else increase ids
             inc = increaseIds by
         in (Clause (inc cHead) (map inc cBody), maximum ids + by + 1)
  where
    collectIds (Var  v)      = [v]
    collectIds (Atom _)      = []
    collectIds (Comp _ args) = concatMap collectIds args

    increaseIds _  (Atom a)         = Atom a
    increaseIds by (Var  v)         = Var (v + by)
    increaseIds by (Comp ftor args) = Comp ftor (map (increaseIds by) args)

