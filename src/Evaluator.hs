module Evaluator where

import Syntax
import Lexer
import Parser
import qualified Environment as E

type VEnv = E.Env Value 

-- This is the values which the program should be able to return
data Value = I Int
           | F Float
           | B Bool
           | Nil
           | LCons Value Value
           | Fun VEnv [Char] [[Char]] Expr
           | P Operator [Value]
           | C Id [Value]
           deriving (Show, Eq)

evaluateString :: String -> Value
evaluateString s = do 
    let Right ts = lexer s
    case parse ts of 
        Right p -> evaluate p
        Left s  -> error s

evaluate :: Program -> Value
evaluate (Bind "main" _ _ body:rest) = evalE E.empty body
evaluate p = error $ show p

evalE :: VEnv -> Expr -> Value 
evalE g (Const (Number (Integer n)))  = I n 
evalE g (Const (Number (Floating n))) = F n 
evalE g (Const (Boole b))             = B b

evalE g (Const (ConstList Empty)) = Nil
evalE g (Const (ConstList l)) = error (show l)
  -- let l' = evalE g l 
  -- in case s of 
    -- Const (Number (Integer n)) -> LCons (I n) l'
    -- s -> error (show s)

evalE g (Prim op) = P op []
evalE g (App a b) = case evalE g a of 
  P op v  -> evalOp op (v ++ [evalE g b])
  C id v  -> C id (v ++ [evalE g b])
  -- Missing function apply
  _       -> error $ "Could not be evaluated"

evalE g e = error $ show e

evalOp :: Operator -> [Value] -> Value
evalOp Add [I x, I y] = I (x + y) 
evalOp Add [F x, F y] = F (x + y) 
evalOp Sub [I x, I y] = I (x - y)
evalOp Sub [F x, F y] = F (x - y)
evalOp Mul [I x, I y] = I (x * y)
evalOp Mul [F x, F y] = F (x * y)
evalOp Div [I x, I y] = I (quot x y)
evalOp Div [F x, F y] = F (x / y)
evalOp Mod [I x, I y] = I (mod x y)

evalOp Not [B b]      = B (not b)
evalOp And [B x, B y] = B (x && y) 
evalOp Or  [B x, B y] = B (x || y)

evalOp Eq  [I x, I y] = B (x == y)
evalOp Eq  [F x, F y] = B (x == y)
evalOp Ne  [I x, I y] = B (not $ x == y)
evalOp Ne  [F x, F y] = B (not $ x == y)
evalOp Gt  [I x, I y] = B (x > y)
evalOp Gt  [F x, F y] = B (x > y)
evalOp Lt  [I x, I y] = B (x < y)
evalOp Lt  [F x, F y] = B (x < y)
evalOp Ge  [I x, I y] = B (x >= y)
evalOp Ge  [F x, F y] = B (x >= y)
evalOp Le  [I x, I y] = B (x <= y)
evalOp Le  [F x, F y] = B (x <= y)
evalOp op vs = P op vs