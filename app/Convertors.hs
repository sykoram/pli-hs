module Convertors(convertTerm, convertProgram, convertGoal) where

import Common
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Parsing
import Utils

{-|
Converts a parsed term into the internal representation, which includes giving variables numerical identifiers.

>>> do t <- Parsing.parseTerm "test(p(X),Y,X,_,_,Z)"; return (convertTerm t (Map.empty,0))
Right ("test"("p"($0), $1, $0, $2, $3, $4),(fromList [("X",0),("Y",1),("Z",4)],5))
-}
convertTerm :: Parsing.PTerm -> (Map.Map String VarId, VarId) -> (ITerm, (Map.Map String VarId, VarId))
convertTerm term (vars,nextId) = case term of
  (Atom a)                      -> (Atom a,             (vars, nextId))
  (Var v) | v == "_"            -> (Var nextId,         (vars, nextId+1)) -- give each anonymous variable unique id
          | v `Map.member` vars -> (Var (vars Map.! v), (vars, nextId))
          | otherwise           -> (Var nextId,         (Map.insert v nextId vars, nextId+1))
  (Comp f args)                 -> (Comp f newArgs, newState)
    where (newArgs, newState) = mapWithState convertTerm (vars,nextId) args

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
      let (head2, state) = convertTerm rHead (Map.empty,0)
          (body2, _)     = mapWithState convertTerm state rBody
      in Map.insertWith (flip (++)) (getSignature head2) [Clause head2 body2] prg

{-|
Converts the goal into the internal representation. Also returns the mapping from variable names onto their ids, and next possible id.

>>> convertGoal [Comp "man" [Var "X"],Comp "man" [Var "Y"],Comp "\\=" [Var "X",Var "Y"]]
(["man"($0),"man"($1),"\\="($0, $1)],(fromList [("X",0),("Y",1)],2))
-}
convertGoal :: Parsing.Goal -> (Goal, (Map.Map String VarId, VarId))
convertGoal = mapWithState convertTerm (Map.empty,0)
