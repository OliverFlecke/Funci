module Evaluator where

import Syntax
import Lexer
import Parser
import qualified Environment as E

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
evalE g (Const n) = n
-- evalE g (Const (Number n)) = n 
-- evalE g (Const (Boole b))             = B b

-- evalE g (Const (ConstList Empty)) = Nil
-- evalE g (Const (ConstList l)) = error (show l)
  -- let l' = evalE g l 
  -- in case s of 
    -- Const (Number (Integer n)) -> LCons (I n) l'
    -- s -> error (show s)


-- evalE g (Prim ListCons) = Listy Empty
-- evalE g (App (App (Prim ListCons) e1) e2) = 
--   case evalE g e1 of 
--     I i -> Cons i (evalE g e2)
--     _   -> error "Only list of integer is supported"


evalE g (Prim op) = P op []
evalE g (App a b) = case evalE g a of 
  P op v  -> evalOp op (v ++ [evalE g b])
  C id v  -> C id (v ++ [evalE g b])
  -- Missing function apply
  _       -> error $ "Could not be evaluated"

evalE g e = error $ show e

evalOp :: Operator -> [Value] -> Value
evalOp Add [Number (I x), Number (I y)] = Number (I $ x + y) 
evalOp Add [Number (F x), Number (F y)] = Number (F $ x + y) 
evalOp Sub [Number (I x), Number (I y)] = Number (I $ x - y)
evalOp Sub [Number (F x), Number (F y)] = Number (F $ x - y)
evalOp Mul [Number (I x), Number (I y)] = Number (I $ x * y)
evalOp Mul [Number (F x), Number (F y)] = Number (F $ x * y)
evalOp Div [Number (I x), Number (I y)] = Number (I $ quot x y)
evalOp Div [Number (F x), Number (F y)] = Number (F $ x / y)
evalOp Mod [Number (I x), Number (I y)] = Number (I $ mod x y)

evalOp Not [Boolean b]      = Boolean (not b)
evalOp And [Boolean x, Boolean y] = Boolean (x && y) 
evalOp Or  [Boolean x, Boolean y] = Boolean (x || y)

evalOp Eq  [Number (I x), Number (I y)] = Boolean (x == y)
evalOp Eq  [Number (F x), Number (F y)] = Boolean (x == y)
evalOp Ne  [Number (I x), Number (I y)] = Boolean (not $ x == y)
evalOp Ne  [Number (F x), Number (F y)] = Boolean (not $ x == y)
evalOp Gt  [Number (I x), Number (I y)] = Boolean (x > y)
evalOp Gt  [Number (F x), Number (F y)] = Boolean (x > y)
evalOp Lt  [Number (I x), Number (I y)] = Boolean (x < y)
evalOp Lt  [Number (F x), Number (F y)] = Boolean (x < y)
evalOp Ge  [Number (I x), Number (I y)] = Boolean (x >= y)
evalOp Ge  [Number (F x), Number (F y)] = Boolean (x >= y)
evalOp Le  [Number (I x), Number (I y)] = Boolean (x <= y)
evalOp Le  [Number (F x), Number (F y)] = Boolean (x <= y)

evalOp ListCons [Listy Empty, v] = Listy (Cons v Empty)
evalOp ListCons [v, Listy l] = Listy (Cons v l) 

evalOp op vs = P op vs