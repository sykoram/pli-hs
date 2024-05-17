module Main where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Parsing
import Terms

-- #TODO: move it somewhere else

type VarId = Int
type ITerm = Term String VarId String

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
convert :: Parsing.PTerm -> (Map.Map String VarId, VarId) -> (ITerm, (Map.Map String VarId, VarId))
convert term (vars,nextId) = case term of
  (Atom a)                      -> (Atom a,             (vars, nextId))
  (Var v) | v == "_"            -> (Var nextId,         (vars, nextId+1)) -- give each anonymous variable unique id
          | v `Map.member` vars -> (Var (vars Map.! v), (vars, nextId))
          | otherwise           -> (Var nextId,         (Map.insert v nextId vars, nextId+1))
  (Comp f args)                 -> (Comp f newArgs, newState)
    where (newArgs, newState) = mapWithState convert (vars,nextId) args

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

-- | variable id -> its value
type Bindings = Map.Map VarId ITerm

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


type Signature = (String, Int)

getSignature :: ITerm -> Signature
getSignature (Atom atom)      = (atom, 0)
getSignature (Comp ftor args) = (ftor, length args)
getSignature (Var _)          = error "Trying to get a signature of a variable."


type Goal = [ITerm]

data Clause = Clause ITerm Goal
  deriving (Show)

-- | predicate signature (name, arity) -> list of Clauses of that predicate
type Program = Map.Map Signature [Clause]

{-
Converts a parsed program (parsed facts and rules) into the internal representation (for each predicate, list of its clauses).

Example: `man(adam). man(david). bros(X,Y):-man(X),man(Y),\=(X,Y).`

>>> convertProgram [Parsing.Fact (Comp "man" [Atom "adam"]), Parsing.Fact (Comp "man" [Atom "david"]), Parsing.Rule (Comp "bros" [Var "X",Var "Y"]) [Comp "man" [Var "X"],Comp "man" [Var "Y"],Comp "\\=" [Var "X",Var "Y"]]]
fromList [(("bros",2),[Clause "bros"($0, $1) ["man"($0),"man"($1),"\\="($0, $1)]]),(("man",1),[Clause "man"("adam") [],Clause "man"("david") []])]
-}
convertProgram :: Parsing.Program -> Program
convertProgram = foldl' step Map.empty
  where
    step prg (Parsing.Fact fHead)       = step prg (Parsing.Rule fHead []) -- fact is a rule with no subgoals
    step prg (Parsing.Rule rHead rBody) =
      let (head2, state) = convert rHead (Map.empty,0)
          (body2, _)     = mapWithState convert state rBody
      in Map.insertWith (flip (++)) (getSignature head2) [Clause head2 body2] prg


{-|
Increases ids of all variables so the least id is now equal to nextId.

>>> renameVars (Clause (Comp "bros" [Var 0,Var 1]) [Comp "man" [Var 0], Comp "man" [Var 1], Comp "\\=" [Var 0,Var 1]]) 10
Clause "bros"($10, $11) ["man"($10),"man"($11),"\\="($10, $11)]
-}
renameVars :: Clause -> VarId -> Clause
renameVars clause@(Clause cHead cBody) nextId = case collectIds cHead ++ concatMap collectIds cBody of
  []  -> clause -- no id => return the original clause
  ids -> let inc = increaseIds (nextId - minimum ids) -- else increase ids
         in Clause (inc cHead) (map inc cBody)
  where
    collectIds (Var  v)      = [v]
    collectIds (Atom _)      = []
    collectIds (Comp _ args) = concatMap collectIds args

    increaseIds _  (Atom a)         = Atom a
    increaseIds by (Var  v)         = Var (v + by)
    increaseIds by (Comp ftor args) = Comp ftor (map (increaseIds by) args)


parseTerm :: String -> (Map.Map String VarId, VarId) -> Either Parsing.ParseError (ITerm, (Map.Map String VarId, VarId))
parseTerm strTerm (vars,nextId) = do
  term <- Parsing.parseTerm strTerm
  return (convert term (vars,nextId))

main :: IO ()
main = putStrLn "Hello, Haskell!"
