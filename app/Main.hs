module Main where

import qualified Parsing
import qualified Data.Map as Map
import Data.List (intercalate, foldl')

-- #TODO: move it somewhere else

type VarId = Int
data Term = Atom [Char] | Var VarId | Comp [Char] [Term]
  deriving (Eq)

instance Show Term where
  show (Atom atom)   = show atom
  show (Var var)     = "#" ++ show var
  show (Comp f args) = show f ++ "(" ++ intercalate ", " (map show args) ++ ")"

{-|
Transforms each element (like `map`), but a state is passed from one call to the next one.

>>> mapWithState (\x state -> (x+state, state+1)) 0 [0..4]
([0,2,4,6,8],5)

>>> take 5 $ fst $ mapWithState (\x st -> (x+st, st+1)) 0 [0..] -- works on infinite lists if the final state is ignored
[0,2,4,6,8]
-}
mapWithState :: (a -> state -> (b, state)) -> state -> [a] -> ([b], state)
mapWithState _ state [] = ([], state)
mapWithState f state (x:xs) =
  let (z, state2) = f x state
      (zs, finalState) = mapWithState f state2 xs
  in (z:zs, finalState)

{-|
Converts a parsed term into the internal representation, which includes giving variables numerical identifiers.

>>> do t <- Parsing.parseTerm "test(p(X),Y,X,_,_,Z)"; return (convert t (Map.empty,0))
Right ("test"("p"(#0), #1, #0, #2, #3, #4),(fromList [("X",0),("Y",1),("Z",4)],5))
-}
convert :: Parsing.Term -> (Map.Map String VarId, VarId) -> (Term, (Map.Map String VarId, VarId))
convert term (vars,nextId) = case term of
  (Parsing.Atom a)                      -> (Atom a,             (vars, nextId))
  (Parsing.Var v) | v == "_"            -> (Var nextId,         (vars, nextId+1)) -- give each anonymous variable unique id
                  | v `Map.member` vars -> (Var (vars Map.! v), (vars, nextId))
                  | otherwise           -> (Var nextId,         (Map.insert v nextId vars, nextId+1))
  (Parsing.Comp f args)                 -> (Comp f newArgs, newState)
    where (newArgs, newState) = mapWithState convert (vars,nextId) args

{-|
Substitutes a value (Term) in for a variable (identified by VarId) in the given Term.

>>> substitute 2 (Atom "xyz") (Comp "p" [Var 2, Var 3, Comp "q" [Var 2]])
"p"("xyz", #3, "q"("xyz"))
-}
substitute :: VarId -> Term -> Term -> Term
substitute var val term = case term of
  (Atom atom)                -> Atom atom
  (Var other) | var == other -> val
              | otherwise    -> Var other
  (Comp ftor args)           -> Comp ftor (map (substitute var val) args)

{-|
Increases ids of all variables so the least id is now equal to nextId

>>> rename (Comp "p" [Var 2, Var 3, Comp "q" [Var 2]]) 10
"p"(#10, #11, "q"(#10))
-}
rename :: Term -> VarId -> Term
rename term nextId = case minId term of -- minId has value => increaseIds; else return the original term
  Nothing  -> term
  Just m   -> increaseIds (nextId - m) term
  where
    minId (Var v)       = Just v
    minId (Atom _)      = Nothing
    minId (Comp _ args) = minimum (map minId args)

    increaseIds _  (Atom a)         = Atom a
    increaseIds by (Var vid)        = Var (vid + by)
    increaseIds by (Comp ftor args) = Comp ftor (map (increaseIds by) args)

-- | variable id -> its value
type Bindings = Map.Map VarId Term

{-|
Creates a binding for a new variable (substitution in existing bindings is also executed).

>>> bind 1 (Atom "a") (Map.fromList [(0,Comp "p" [Var 1])])
fromList [(0,"p"("a")),(1,"a")]
-}
bind :: VarId -> Term -> Bindings -> Bindings
bind var val bindings
  | var `Map.notMember` bindings = Map.insert var val (Map.map (substitute var val) bindings) -- replace the newly binded variable in the values with its value
  | otherwise                    = error "Binding a variable that has already been binded."

{-|
Applies all binding onto the given term.

>>> applyBindings (Map.fromList [(0,Atom "a")]) (Comp "p" [Var 0,Var 1])
"p"("a", #1)
-}
applyBindings :: Bindings -> Term -> Term
applyBindings bindings term = case term of
  (Atom a)      -> Atom a
  (Var v)       -> Map.findWithDefault (Var v) v bindings
  (Comp f args) -> Comp f (map (applyBindings bindings) args)

{-|
Unifies the two terms and updates the bindings. Returns Nothing on failure.

Example: unify `p(q(a),X0,q(X1))` and `p(X2,X3,X3)` --> `X0 = X3 = q(X1), X2 = q(a)`

>>> let t1 = Comp "p" [Comp "q" [Atom "a"],Var 0,Comp "q" [Var 1]] ; t2 = Comp "p" [Var 2,Var 3,Var 3] in unify t1 t2 Map.empty
Just (fromList [(0,"q"(#1)),(2,"q"("a")),(3,"q"(#1))])

Example: unify `p(X0,X0,X0)` and `p(X1,q(X2),q(q(X3)))` --> `X0 = q(q(X3)), X1 = q(q(X3)), X2 = q(X3)`

>>> let t1 = Comp "p" [Var 0,Var 0,Var 0] ; t2 = Comp "p" [Var 1,Comp "q" [Var 2],Comp "q" [Comp "q" [Var 3]]] in unify t1 t2 Map.empty
Just (fromList [(0,"q"("q"(#3))),(1,"q"("q"(#3))),(2,"q"(#3))])
-}
unify :: Term -> Term -> Bindings -> Maybe Bindings
unify term1 term2 bindings = case (applyBindings bindings term1, applyBindings bindings term2) of -- apply bindings before unifying => binded variables shouldn't occur in the terms afterwards
  (Atom a, Atom b) | a == b    -> Just bindings
  (Var u,  Var v)  | u == v    -> Just bindings
                   | u < v     -> Just (bind u (Var v) bindings) -- order the variables (maybe not necessary)
                   | otherwise -> Just (bind v (Var u) bindings)
  (Var v,  term)   -> Just (bind v term bindings)
  (term,   Var v)  -> Just (bind v term bindings)
  (Comp f1 args1, Comp f2 args2) | sameComp -> foldl' unifyArgsStep (Just bindings) (zip args1 args2) -- unify arguments one after each other
    where
      sameComp = f1 == f2 && length args1 == length args2 -- the same functor and arity
      unifyArgsStep maybeBindings (a1, a2) = maybeBindings >>= unify a1 a2 -- calls unify if maybeBindings has a value (is Just)
  (_, _) -> Nothing -- cannot unify different Atoms, an Atom with a Comp or Comps with a different functor or arity


main :: IO ()
main = putStrLn "Hello, Haskell!"
