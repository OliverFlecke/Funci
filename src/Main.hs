module Main where

import Lexer 
import Parser
import Syntax 
import Evaluator

main :: IO ()
main = do 
  putStrLn "Enter program: "
  s <- getLine 
  let Right p = parseString s 
  print p
  let e = evaluate p 
  print e 
  -- putStrLn "End of program"
