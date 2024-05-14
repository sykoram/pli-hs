{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parsing (Term(..), Goal, Clause(..), Program, Query, parseProgram, parseQuery, parseTerm) where

import Text.Parsec
import Text.Parsec.String

----------
-- TERM --
----------

data Term = Atom [Char] | Var [Char] | Comp [Char] [Term]
  deriving (Eq, Show)

-- | Parses a string quated in `'`. The characters `\` and `'` must be escaped. Example: `'Darth Vader'`
quotedStr :: Parser [Char]
quotedStr = do
  char '\''
  str <- many (noneOf "\\'" <|> (char '\\' >> oneOf ['\\', '\'']))
  char '\''
  return str

-- | Parses an atom starting with a lowercase letter and containing only alphanumeric characters and `_`. Example: `list_length2`
basicAtom :: Parser [Char]
basicAtom = do
  c1 <- lower
  chars <- many (choice [alphaNum, char '_'])
  return (c1:chars)

-- | Parses a special atom, eg. `=`.
specialAtom :: Parser [Char]
specialAtom = choice (map (try . string) specialAtoms)
  where specialAtoms = ["=", "\\=", "\\+", "[]", ".", "0"]

-- | Parses an atom.
atom :: Parser [Char]
atom = try basicAtom <|> try specialAtom <|> quotedStr

-- | Parses a variable (which must start with a uppercase letter or a `_` and contain only alphanumeric characters and `_`). Example: `Length`
var :: Parser [Char]
var = do
  c1 <- upper <|> char '_'
  chars <- many (choice [alphaNum, char '_'])
  return (c1:chars)

-- | Parses a compound term, which includes a functor name and a list of comma-separated arguments (which are terms). Example: `list_length2( .(1,.(2,[])) , Length)`
compoundTerm :: Parser Term
compoundTerm = do
  functor <- atom
  char '('
  spaces
  arg1 <- term -- one argument is obligatory
  argTail <- many (try (spaces >> char ',' >> spaces >> term)) -- more than one are optional
  spaces
  char ')'
  return (Comp functor (arg1:argTail))

-- | Parses a term (a compound term, an atom or a variable).
term :: Parser Term
term = try compoundTerm <|> try (Atom <$> atom) <|> Var <$> var

parseTerm :: String -> Either ParseError Term
parseTerm = parse term ""


------------
-- CLAUSE --
------------

type Goal = [Term]
data Clause = Fact Term | Rule Term Goal
  deriving (Eq, Show)

-- | Parses a fact, which is a term followed by `.`.
fact :: Parser Clause
fact = do
  f <- term
  spaces
  char '.'
  return (Fact f)

-- | Parses a goal, which is a collection of terms delimited by `,`.
goal :: Parser Goal
goal = do
  t1 <- term
  terms <- many (try (spaces >> char ',' >> spaces >> term))
  return (t1:terms)

-- | Parses a rule, which has a head (term) followed by `:-` followed by a body (goal) and ending with `.`.
rule :: Parser Clause
rule = do
  h <- term
  spaces
  string ":-"
  spaces
  b <- goal
  spaces
  char '.'
  return (Rule h b)

-- | Parses a clause (a fact or a rule).
clause :: Parser Clause
clause = try fact <|> rule


--------------------
-- PROGRAM, QUERY --
--------------------

type Program = [Clause]
type Query = Goal

-- | Parses a program, which is a collection of clauses.
program :: Parser Program
program = many (try (spaces >> clause)) <* spaces <* eof

-- | Parses a program, which is a collection of clauses.
parseProgram :: String -> Either ParseError Program
parseProgram = parse program ""

-- | Parses a query, which is a goal ending with `.`.
query :: Parser Query
query = spaces >> goal <* spaces <* char '.' <* eof

-- | Parses a query, which is a goal ending with `.`.
parseQuery :: String -> Either ParseError Query
parseQuery = parse query ""
