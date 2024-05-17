module Terms (Term(..)) where

import Data.List (intercalate)

data Term atom var ftor = Atom atom | Var var | Comp ftor [Term atom var ftor]

instance (Show atom, Show var, Show ftor) => Show (Term atom var ftor) where
  show (Atom atom)   = show atom
  show (Var var)     = "$" ++ show var
  show (Comp f args) = show f ++ "(" ++ intercalate ", " (map show args) ++ ")"
