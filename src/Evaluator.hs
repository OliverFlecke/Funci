module Evaluator where

import Syntax
import HandLexer
import HandParser

-- This is the values which the program should be able to return
data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | F VEnv [Char] [[Char]] Exp
           deriving (Show)

evaluate :: Program -> Value
evaluate = undefined