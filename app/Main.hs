module Main where

import Common
import Convertors
import Data.Maybe
import qualified Data.Map as Map
import Operations
import qualified Parsing
import Utils

-- #TODO: refactor; move somewhere else

unRight :: Either a b -> b
unRight (Right x) = x
unRight (Left  _) = error "Expected Right, got Left"

data State = State Bindings VarId
  deriving (Show)

testProgram :: Program
testProgram = convertProgram $ unRight $ Parsing.parseProgram "\
\true :- =(a,a).\
\false :- =(a,b).\

\len([], 0).\
\len(.(_,Xs), s(L)) :- len(Xs,L).\
\"

testQuery :: Query; testQueryVars :: Map.Map String VarId; testNextId :: VarId
(testQuery, (testQueryVars, testNextId)) = convertQuery $ unRight $ Parsing.parseQuery "len(.(a,[]),Length)."

testState :: State
testState = State Map.empty testNextId

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


{-
#TODO
SATISFY PREDICATE:
1. najdeme si klauzule, na které matchujeme
2. pro jednu konkrétní:
   - zkoušíme postupně splnit podcíle
   - když se nějaký nepodaří, tak zkoušíme splnit předchozí jinak
   - když už nelze jinak splnit úplně první podcíl, tak zkusíme jinou klauzuli (goto 2)

CUTS:
- we went over a cut and failed (totally) on the very next subgoal
  => no backtracking into subgoals before the cut
  => no trying of different clauses
- multiple cuts is probably allowed!
- cut => some different handling (for each subgoal and also clause, return also information whether we can backtrack?)
-}


satisfySubgoal :: ITerm -> State -> Program -> [State]
-- unification (=) is built-in:
satisfySubgoal (Comp "=" [x, y]) (State bindings nextId) _ = maybeToList $ do
  bindings1 <- unify x y bindings
  return (State bindings1 nextId)
-- other subgoals:
satisfySubgoal term state program =
  let
    matches = matchingClauses term state program
  in do -- to support cuts, we'll need to filter the results (stop them after a signal)
    (Clause _ body, state1) <- matches -- for each match...
    satisfyGoal body state1 program    -- ...satisfy its body

satisfyGoal :: Goal -> State -> Program -> [State]
satisfyGoal [] state _ = [state] -- empty goal - nothing more to satisfy
satisfyGoal (g:gs) state program =
  let
    subgoalSatisfactions = satisfySubgoal g state program
  in do -- to support cuts, we'll need to filter the results (stop them after a signal)
    state1 <- subgoalSatisfactions
    satisfyGoal gs state1 program


convertBindingsBack :: Map.Map String VarId -> Bindings -> Map.Map String Parsing.PTerm
convertBindingsBack varIds bindings = Map.mapKeys toVarName $ Map.map toPTerm $ Map.intersection bindings varNames
  where
    varNames = flipMap varIds
    toVarName = (Map.!) varNames

    toPTerm (Atom a)         = Atom a
    toPTerm (Var v)          = Var (toVarName v)
    toPTerm (Comp ftor args) = Comp ftor (map toPTerm args)


parseAndSolveQuery :: String -> Program -> Either Parsing.ParseError [Map.Map String Parsing.PTerm]
parseAndSolveQuery queryStr program =
  do
    (query, (varIds, nextId)) <- convertQuery <$> Parsing.parseQuery queryStr
    let resultStates = satisfyGoal query (State Map.empty nextId) program
    let bindings = map (\(State b _) -> convertBindingsBack varIds b) resultStates
    return bindings


main :: IO ()
main = putStrLn "Hello, Haskell!"
