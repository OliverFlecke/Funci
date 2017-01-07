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
      printValue $ evaluateString program

interactive :: IO () 
interactive = do 
  putStr ">> "
  hFlush stdout
  expression <- getLine 
  if expression == "exit"
  then exitSuccess
  else do 
    printValue $ evaluateString expression
    interactive