module Main where

import System.Environment
import System.Directory
import System.IO
import System.Exit
import Evaluator
import Syntax

main :: IO ()
main = do
  args <- getArgs
  case args of 
    []        -> interactive
    file:rest -> do 
      putStrLn $ file
      program <- readFile file
      case evaluateString program of 
        Left s  -> errorHandling s 
        Right v -> printValue v

interactive :: IO () 
interactive = do 
  putStr ">> "
  hFlush stdout
  expression <- getLine 
  if expression == "exit"
  then exitSuccess
  else do 
    case evaluateString expression of 
      Left s  -> errorHandling s
      Right v -> printValue v
    interactive

errorHandling :: Show a => Exception a -> IO () 
errorHandling DivideByZero        = putStrLn "Divide by zero is not allowed"
errorHandling (LexingError s)     = putStrLn $ "Lexing error: " ++ s
errorHandling (ParsingError s)    = putStrLn $ "Parsing error: " ++ s
errorHandling (EvaluatorError s)  = putStrLn $ "Evaluator error: " ++ s
errorHandling (ScopeError x g)    = putStrLn $ "Variable '" ++ x ++ "' not in scope in environment " ++ (show g)
errorHandling (TypeError x y)     = putStrLn $ "Types are not matching: " ++ (show x) ++ " should be " ++ (show y)
errorHandling e = undefined