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
  let e = evaluate p 
  print e 
  -- putStrLn "End of program"
