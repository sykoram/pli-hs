{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parsing (PTerm, Goal, Clause(..), Program, Query, parseProgram, parseQuery, parseTerm, ParseError) where

import TermBase
import Text.Parsec
import Text.Parsec.String

----------
-- TERM --
----------

type PTerm = Term String String String

-- | Parses a string quated in `'`. The characters `\` and `'` must be escaped. Example: `'Darth Vader'`
quotedStr :: Parser String
quotedStr = do
  char '\''
  str <- many (noneOf "\\'" <|> (char '\\' >> oneOf ['\\', '\'']))
  char '\''
  return str

-- | Parses an atom starting with a lowercase letter and containing only alphanumeric characters and `_`. Example: `list_length2`
basicAtom :: Parser String
basicAtom = do
  c1 <- lower
  chars <- many (choice [alphaNum, char '_'])
  return (c1:chars)

-- | Parses a special atom, eg. `=`.
specialAtom :: Parser String
specialAtom = choice (map (try . string) specialAtoms)
  where specialAtoms = ["=", "\\=", "\\+", "[]", ".", "0"]

-- | Parses an atom.
atom :: Parser String
atom = try basicAtom <|> try specialAtom <|> quotedStr

-- | Parses a variable (which must start with a uppercase letter or a `_` and contain only alphanumeric characters and `_`). Example: `Length`
var :: Parser String
var = do
  c1 <- upper <|> char '_'
  chars <- many (choice [alphaNum, char '_'])
  return (c1:chars)

-- | Parses a compound term, which includes a functor name and a list of comma-separated arguments (which are terms). Example: `list_length2( .(1,.(2,[])) , Length)`
compoundTerm :: Parser PTerm
compoundTerm = do
  functor <- atom
  char '('
  spaces
  args <- try term `sepBy` try (spaces >> char ',' >> spaces)
  spaces
  char ')'
  return (Comp functor args)

-- | Parses a compund term or an atom, but not a variable.
compoundOrAtom :: Parser PTerm
compoundOrAtom = try compoundTerm <|> (Atom <$> atom)

-- | Parses a term (a compound term, an atom or a variable).
term :: Parser PTerm
term = try compoundOrAtom <|> (Var <$> var)

parseTerm :: String -> Either ParseError PTerm
parseTerm = parse term ""


------------
-- CLAUSE --
------------

type Goal = [PTerm]
data Clause = Fact PTerm | Rule PTerm Goal
  deriving (Show)

-- | Parses a fact, which is a term followed by `.`.
fact :: Parser Clause
fact = do
  f <- compoundOrAtom
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
  h <- compoundOrAtom
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
