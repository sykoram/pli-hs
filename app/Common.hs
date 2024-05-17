module Common (Term(..), mapWithState) where

import Data.List (intercalate)

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


data Term atom var ftor = Atom atom | Var var | Comp ftor [Term atom var ftor]

instance (Show atom, Show var, Show ftor) => Show (Term atom var ftor) where
  show (Atom atom)   = show atom
  show (Var var)     = "$" ++ show var
  show (Comp f args) = show f ++ "(" ++ intercalate ", " (map show args) ++ ")"
