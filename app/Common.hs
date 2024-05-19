module Common (Term(..), VarId, ITerm, Goal, Clause(..), Signature, Program, Query, Bindings, getSignature) where

import qualified Data.Map as Map
import TermBase

type VarId = Int
type ITerm = Term String VarId String

type Goal = [ITerm]
data Clause = Clause ITerm Goal
  deriving (Show)

type Signature = (String, Int)

-- | predicate signature (name, arity) -> list of Clauses of that predicate
type Program = Map.Map Signature [Clause]

type Query = Goal

-- | variable id -> its value
type Bindings = Map.Map VarId ITerm

{-|
Returns the signature of a term, which is its name and its arity.

>>> getSignature (Comp "pred" [Var 0,Atom "a"])
("pred",2)
-}
getSignature :: ITerm -> Signature
getSignature (Atom atom)      = (atom, 0)
getSignature (Comp ftor args) = (ftor, length args)
getSignature (Var _)          = error "Trying to get a signature of a variable."
