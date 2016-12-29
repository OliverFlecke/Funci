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
evalE g (Var x)   = 
  case E.lookup g x of 
    Just n  -> n
    Nothing -> error $ "Variable was not in the environment" ++ (show g)

evalE g (LetIn ((x, e):[]) b) = let g' = E.add g (x, evalE g e) 
                                in evalE g' b
evalE g (LetIn ((x, e):xs) b) = let g' = E.add g (x, evalE g e)
                                in evalE g' (LetIn xs b)

evalE g (IfThenElse b t f) =
  case evalE g b of 
    Boolean True  -> evalE g t 
    Boolean False -> evalE g f
    _             -> error $ (show b) ++ " - This should return a boolean value"

evalE g (Prim op) = P op []
evalE g (App a b) = case evalE g a of 
  P op v  -> evalOp op (v ++ [evalE g b])
  C id v  -> C id (v ++ [evalE g b])
  -- Missing function apply
  _       -> error $ "Could not be evaluated"

evalE g e = error $ show e

-- Evaluate operators
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
evalOp ListCons [Listy l,     v] = Listy (Cons v l) 

evalOp op vs = P op vs