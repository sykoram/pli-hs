import Text.Parsec
import Text.Parsec.String

data Term = Atom [Char] | Var [Char] | Compound [Char] [Term]
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
  tail <- many (choice [alphaNum, char '_'])
  return (c1:tail)

-- | Parses a special atom, eg. `=`
specialAtom :: Parser [Char]
specialAtom = string "=" <|> string "."

-- | Parses an atom.
atom :: Parser [Char]
atom = try basicAtom <|> try specialAtom <|> quotedStr

-- | Parses a variable (which must start with a uppercase letter or a `_` and contain only alphanumeric characters and `_`). Example: `Length`
var :: Parser [Char]
var = do
  c1 <- upper <|> char '_'
  tail <- many (choice [alphaNum, char '_'])
  return (c1:tail)  

-- | Parses a compound term, which includes a functor name and a list of comma-separated arguments (which are terms). Example: `list_length2(s(s(nil)), Length)`
compoundTerm :: Parser Term
compoundTerm = do
  functor <- atom
  char '('
  spaces
  arg1 <- term -- one argument is obligatory
  argTail <- many (try (spaces >> char ',' >> spaces >> term)) -- more than one are optional
  spaces
  char ')'
  return (Compound functor (arg1:argTail))

-- | Parses a term (a compound term, an atom or a variable).
term :: Parser Term
term = try compoundTerm <|> try (Atom <$> atom) <|> Var <$> var

