module Main where

import qualified Parsing
import qualified Data.Map as Map
import Data.List (intercalate)

-- #TODO: move it somewhere else

data Term = Atom [Char] | Var Int | Compound [Char] [Term]
  deriving (Eq)

instance Show Term where
  show (Atom atom)       = show atom
  show (Var var)         = "#" ++ show var
  show (Compound f args) = show f ++ "(" ++ intercalate ", " (map show args) ++ ")"

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

>>> let (Right term) = Parsing.parseTerm "test(p(X),p(Y),X)" in convert term (Map.empty, 0)
("test"("p"(#0), "p"(#1), #0),(fromList [("X",0),("Y",1)],2))
-}
convert :: Parsing.Term -> (Map.Map String Int, Int) -> (Term, (Map.Map String Int, Int))
convert term (vars,nextId) = case term of
  (Parsing.Atom a)                      -> (Atom a,             (vars,nextId))
  (Parsing.Var v) | v `Map.member` vars -> (Var (vars Map.! v), (vars,nextId))
                  | otherwise           -> (Var nextId,         (Map.insert v nextId vars, nextId+1))
  (Parsing.Compound f args)             -> (Compound f newArgs, newState)
    where (newArgs, newState) = mapWithState convert (vars,nextId) args

{-|
Substitutes a value (Term) in for a variable (identified by Int) in the given Term.

>>> substitute 2 (Atom "xyz") (Compound "p" [Var 2, Var 3, Compound "q" [Var 2]])
"p"("xyz", #3, "q"("xyz"))
-}
substitute :: Int -> Term -> Term -> Term
substitute var val term = case term of
  (Atom atom)                -> Atom atom
  (Var other) | var == other -> val
              | otherwise    -> Var other
  (Compound ftor args)       -> Compound ftor (map (substitute var val) args)

{-|
Increases ids of all variables so the least id is now equal to nextId

>>> rename (Compound "p" [Var 2, Var 3, Compound "q" [Var 2]]) 10
"p"(#10, #11, "q"(#10))
-}
rename :: Term -> Int -> Term
rename term nextId = case minId term of -- minId has value => increaseIds; else return the original term
  Nothing  -> term
  Just m   -> increaseIds (nextId - m) term
  where
    minId (Var vid) = Just vid
    minId (Atom _) = Nothing
    minId (Compound _ args) = minimum (map minId args)

    increaseIds _  (Atom a) = Atom a
    increaseIds by (Var vid) = Var (vid + by)
    increaseIds by (Compound ftor args) = Compound ftor (map (increaseIds by) args)


main :: IO ()
main = putStrLn "Hello, Haskell!"
