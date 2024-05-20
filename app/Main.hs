module Main where

import Common
import Convertors
import qualified Data.Map as Map
import qualified Parsing
import Utils
import Satisfaction

unRight :: Either a b -> b
unRight (Right x) = x
unRight (Left  _) = error "Expected Right, got Left"

testProgram :: Program
testProgram = convertProgram $ unRight $ Parsing.parseProgram "\
\p(a).\
\p(b).\
\p(c).\

\cut(X,Y) :- p(X),!,p(Y).\
\cut(no,no).\

\cut(X,Y,Z) :- p(X),p(Y),!,p(Z).\
\cut(no,no,no).\

\cut2(X,Y,Z) :- p(X),!,p(Y),p(Z).\
\cut2(no,no,no).\

\repeat.\
\repeat :- repeat.\

\q(a).\
\q(b) :- !.\
\q(c).\

\cut_fail(no) :- !, =(a,b).\
\cut_fail(also_no).\

\fail_cut(a) :- =(a,b), !.\
\fail_cut(b).\

\false :- =(a,b).\

\\\+(Goal) :- Goal, !, false.\
\\\+(Goal).\

\\\=(X,Y) :- \\+(=(X,Y)).\
\"

testQuery :: Query; testQueryVars :: Map.Map String VarId; testNextId :: VarId
(testQuery, (testQueryVars, testNextId)) = convertQuery $ unRight $ Parsing.parseQuery "cut(X,Y,Z)."

testState :: State
testState = State Map.empty testNextId


-- | Gives variables their names back (in both keys and inside values).
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
    let (resultStates, _) = satisfyGoal query (State Map.empty nextId) program
    let bindings = map (\(State b _) -> convertBindingsBack varIds b) resultStates
    return bindings


main :: IO ()
main = putStrLn "Hello, Haskell!"
