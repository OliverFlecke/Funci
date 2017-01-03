module Main where

import System.Environment
import System.Directory
import Evaluator

main :: IO ()
main = do
  file:args <- getArgs
  putStrLn $ file
  program <- readFile file
  print $ evaluateString program
