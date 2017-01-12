module Main where

import System.Environment
import System.Directory
import System.IO
import System.Exit
import Data.Text
import Evaluator
import qualified Environment as E 
import Syntax

main :: IO ()
main = do
  args <- getArgs
  case args of 
    []        -> interactive E.empty
    file:rest -> do 
      putStrLn $ file
      program <- readFile file
      case evaluateString program of 
        Left s  -> errorHandling s 
        Right v -> printValue v

interactive :: (Read a, Show a, RealFrac a) => VEnv a -> IO () 
interactive g = do 
  putStr ">> "
  hFlush stdout
  expression <- getLine 
  if (unpack (strip $ pack expression)) == "exit"
  then exitSuccess
  else do 
    case evaluateStringWithEnv g expression of 
      Left s  -> errorHandling s
      Right f@(Fun x _ _ _) -> let g' = E.add g (x, f)
                                in interactive g' 
      Right v -> printValue v
    interactive g

errorHandling :: Show a => Exception a -> IO () 
errorHandling DivideByZero        = putStrLn "Divide by zero is not allowed"
errorHandling (LexingError s)     = putStrLn $ "Lexing error: " ++ s
errorHandling (ParsingError s)    = putStrLn $ "Parsing error: " ++ s
errorHandling (EvaluatorError s)  = putStrLn $ "Evaluator error: " ++ s
errorHandling (ScopeError x g)    = putStrLn $ "Variable '" ++ x ++ "' not in scope in environment " ++ (show g)
errorHandling (TypeError x y)     = putStrLn $ "Types are not matching: " ++ (show x) ++ " should be " ++ (show y)
errorHandling e = undefined