module Main where

import Common
import Convertors
import qualified Data.Map as Map
import qualified Parsing
import Utils
import Satisfaction
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Data.List (intercalate)
import Data.Maybe

-- | Gives variables their names back (in both keys and inside values).
convertBindingsBack :: Map.Map String VarId -> Bindings -> Map.Map String Parsing.PTerm
convertBindingsBack varIds bindings = Map.mapKeys toVarName $ Map.map toPTerm $ Map.intersection bindings varNames
  where
    varNames = flipMap varIds
    toVarName x = fromMaybe ("__"++show x) $ Map.lookup x varNames

    toPTerm (Atom a)         = Atom a
    toPTerm (Var v)          = Var (toVarName v)
    toPTerm (Comp ftor args) = Comp ftor (map toPTerm args)

-- | Parses the given string as a query, and obtains its satisfactions/results.
parseAndSolveQuery :: String -> Program -> Either Parsing.ParseError [Map.Map String Parsing.PTerm]
parseAndSolveQuery queryStr program =
  do
    (query, (varIds, nextId)) <- convertQuery <$> Parsing.parseQuery queryStr
    let (resultStates, _) = satisfyGoal query (State Map.empty nextId) program
    let bindings = map (\(State b _) -> convertBindingsBack varIds b) resultStates
    return bindings

promptLine :: String -> IO String
promptLine msg = do
  putStr msg
  hFlush stdout
  getLine

showBindings :: Map.Map String Parsing.PTerm -> String
showBindings bindings
  | Map.null bindings = "True"
  | otherwise         = intercalate "\n" $ map (\(var,term) -> var ++ " = " ++ show term) (Map.toAscList bindings)

-- | Prints the (first) result and waits for the user to say whether to print the next one ([Enter] or ";"+[Enter] to print the next one; "."+[Enter] to stop).
printQueryResults :: [Map.Map String Parsing.PTerm] -> IO ()
printQueryResults []               = putStrLn "False."
printQueryResults [result]         = putStr (showBindings result) >> putStrLn "."
printQueryResults (result:results) = do
  putStr $ showBindings result
  next <- toDisplayNext
  if next
    then putStrLn ";" >> printQueryResults results
    else putStrLn "."

  where
    toDisplayNext :: IO Bool
    toDisplayNext = do
      s <- promptLine " "
      case s of
        ""  -> return True
        ";" -> return True
        "." -> return False
        _   -> toDisplayNext

-- | Runs the read-eval-print loop indefinitely.
repl :: Program -> IO ()
repl program = do
  queryString <- promptLine "\n?- "
  case parseAndSolveQuery queryString program of
    Left  err     -> print err
    Right results -> printQueryResults results
  repl program

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStrLn "Provide exactly one path to a Prolog program file."
  else do
    programString <- readFile $ head args
    case convertProgram <$> Parsing.parseProgram programString of
      Left  err     -> print err
      Right program -> repl program
