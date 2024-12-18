module Satisfaction(matchingClauses, satisfySubgoal, satisfyGoal) where

import Common
import Data.Maybe
import qualified Data.Map as Map
import Operations
import Utils
import Data.List

{-|
Returns clauses for which unification of the given term with their head succeeded. (The resulting bindings are also returned.)

Example:
  term = `parent($0,$1)`;
  bindings = `(1,Atom "abel")`;
  program = `parent(adam,kain). parent(adam,abel). parent(eva,kain). parent(eva,abel).`

> let term = Comp "parent" [Var 0, Var 1]
>     bindings = Map.fromList [(1, Atom "abel")]
>     nextId = 2
>     program = Map.fromList [(("parent",2), [Clause (Comp "parent" [Atom "adam",Atom "kain"]) [],
>                                             Clause (Comp "parent" [Atom "adam",Atom "abel"]) [],
>                                             Clause (Comp "parent" [Atom "eva", Atom "kain"]) [],
>                                             Clause (Comp "parent" [Atom "eva", Atom "abel"]) []])]
> in matchingClauses term (State bindings nextId) program
> == [(Clause "parent"("adam", "abel") [],State (fromList [(0,"adam"),(1,"abel")]) 2),
>     (Clause "parent"("eva",  "abel") [],State (fromList [(0,"eva"), (1,"abel")]) 2)]
-}
matchingClauses :: ITerm -> State -> Program -> [(Clause, State)]
matchingClauses term (State bindings nextId) program =
  let
    signature = getSignature term
    allPredicateClauses = fromMaybe [] $ Map.lookup signature program
    renamedClausesPlusNextIds = map (`renameVars` nextId) allPredicateClauses
  in do
      (clause@(Clause cHead _), nextId1) <- renamedClausesPlusNextIds
      bindings1 <- maybeToList $ unify cHead term bindings
      return (clause, State bindings1 nextId1)
-- #TODO: empty is not good

{-|
Returns satisfactions of the given subgoal (a term).

Example: subgoal `p(X0)`, program `p(a). p(b).`

>>> satisfySubgoal (Comp "p" [Var 0]) (State Map.empty 1) (Map.fromList [(("p",1),[Clause (Comp "p" [Atom "a"]) [],Clause (Comp "p" [Atom "b"]) []])])
[State (fromList [(0,"a")]) 1,State (fromList [(0,"b")]) 1]
-}
satisfySubgoal :: ITerm -> State -> Program -> [State]
-- unification (=) is built-in:
satisfySubgoal (Comp "=" [x, y]) (State bindings nextId) _ = maybeToList $ do
  bindings1 <- unify x y bindings
  return (State bindings1 nextId)
-- variable => replace it with its binded value (will loop indefinitely if not binded!)
satisfySubgoal (Var v) state@(State bindings _) program = satisfySubgoal (Map.findWithDefault (Var v) v bindings) state program
-- other subgoals:
satisfySubgoal term state program =
  let
    matches = matchingClauses term state program -- get matching clauses (subgoal unified with the head)
    satisfactions = map (\(Clause _ body, state1) -> satisfyGoal body state1 program) matches -- satisfy the matches by satisfying their bodies
    satisfactionsUntilCut = takeWhilePlus1 snd satisfactions -- if there was a cut, throw away the subsequent alternatives
  in concatMap fst satisfactionsUntilCut

{-|
Returns satisfactions of the given goal (a list of terms).
(Also returns whether trying other alternatives is allowed - this is False when there was a cut at some point.)

Example: goal `p(X0),p(X1)`, program `p(a). p(b).`

>>> satisfyGoal [Comp "p" [Var 0],Comp "p" [Var 1]] (State Map.empty 2) (Map.fromList [(("p",1),[Clause (Comp "p" [Atom "a"]) [],Clause (Comp "p" [Atom "b"]) []])])
([State (fromList [(0,"a"),(1,"a")]) 2,State (fromList [(0,"a"),(1,"b")]) 2,State (fromList [(0,"b"),(1,"a")]) 2,State (fromList [(0,"b"),(1,"b")]) 2],True)
-}
satisfyGoal :: Goal -> State -> Program -> ([State],Bool)
-- empty goal => nothing more to satisfy
satisfyGoal []            state _       = ([state], True)
-- cut => keep on satisfying, but return information to stop trying other alternatives
satisfyGoal (Atom "!":gs) state program = (fst $ satisfyGoal gs state program, False)
-- other subgoal => satisfy it and continue recursively
satisfyGoal (g       :gs) state program =
  let
    subgoalSatisfactions = satisfySubgoal g state program -- satisfy the subgoal
    satisfactions = map (\state1 -> satisfyGoal gs state1 program) subgoalSatisfactions -- for each satisfaction independently, satisfy the following subgoals
    satisfactionsUntilCut = takeWhilePlus1 snd satisfactions -- take all results until we get information that we went over a cut
    allowOtherAlternatives = maybe True snd (find (\(sat,allow) -> not allow || not (null sat)) satisfactionsUntilCut)
                             -- can we allow other alternatives? - do it according to either the first False value (then its definitely NO because there was a cut)
                             --                                                       or the first *non-empty* satisfaction (it must have gone over a cut if there was one)
                             -- I hope this is correct; it should even work with some "infinite" predicates like `repeat`
  in case satisfactionsUntilCut of
    [] -> ([], True) -- failed to satisfy this subgoal
    _  -> (concatMap fst satisfactionsUntilCut, allowOtherAlternatives)
