module Evaluator (
  evaluateString, 
  evaluate
  ) where

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
evaluate p = evalP E.empty p

evalP :: VEnv -> Program -> Value 
evalP g [] = 
  case E.lookup g "main" of 
    Just (Fun g' [] e)  -> evalE g e
    Nothing             -> error $ "Could not find the main function" ++ (show g)
evalP g (Bind f _ vs e : rest) = 
  let g' = E.add g (f, Fun E.empty vs e)
  in evalP g' rest

-- Evaluate an expression
evalE :: VEnv -> Expr -> Value 
evalE g (Const n) = n
evalE g (Var x)   = 
  case E.lookup g x of 
    Just (Fun g' [] e)  -> evalE (E.union g' g) e
    Just (Fun g' vs e)  -> Fun (E.union g' g) vs e
    Just n              -> n
    Nothing -> error $ "Variable was not in the environment. \nVar: " ++ (show x) ++ "\nEnv: " ++ (show g)

evalE g (LetIn ((Bind x _ ids e):[]) b) = let g' = E.add g (x, Fun g ids e) 
                                in evalE g' b
evalE g (LetIn ((Bind x _ ids e):xs) b) = let g' = E.add g (x, Fun g ids e)
                                in evalE g' (LetIn xs b)

evalE g (IfThenElse b t f) =
  case evalE g b of 
    Boolean True  -> evalE g t 
    Boolean False -> evalE g f
    _             -> error $ (show b) ++ " - This should return a boolean value"

evalE g (Prim op) = P op []
evalE g (App (App (Prim Sub) x) y) = 
  case (evalE g x, evalE g y) of 
    (Number (I x') _, Number (I y') _)  -> Number (I (x' - y')) Nothing
    (Number (F x') _, Number (F y') _)  -> Number (F (x' - y')) Nothing
evalE g (App a b) = case evalE g a of 
  P op v          -> evalOp op (v ++ [evalE g b])
  C id v          -> C id (v ++ [evalE g b])
  Fun g' (v:[]) e -> let g'' = E.add g' (v, evalE g b)
                      in evalE g'' e
  Fun g' (v:vs) e -> let g'' = E.add g' (v, evalE g b) 
                      in Fun g'' vs e
  t               -> error $ "Could not be evaluated: " ++ (show t) ++ "\n" ++ (show a) ++ "\n" ++ (show b)

evalE g e = error $ show e

-- Evaluate operators
evalOp :: Operator -> [Value] -> Value
evalOp Sub [Number (I x) unit]                = Number (I $ -x) unit
evalOp Add [Number (I x) u, Number (I y) u']  = Number (I $ x + y) (checkUnits u u') 
evalOp Add [Number (F x) u, Number (F y) u']  = Number (F $ x + y) (checkUnits u u') 
evalOp Sub [Number (I x) u, Number (I y) u']  = Number (I $ x - y) (checkUnits u u')
evalOp Sub [Number (F x) u, Number (F y) u']  = Number (F $ x - y) (checkUnits u u')
evalOp Mul [Number (I x) u, Number (I y) u']  = Number (I $ x * y) (checkUnits u u')
evalOp Mul [Number (F x) u, Number (F y) u']  = Number (F $ x * y) (checkUnits u u')
evalOp Div [Number (I x) u, Number (I y) u']  = Number (I $ quot x y) (checkUnits u u')
evalOp Div [Number (F x) u, Number (F y) u']  = Number (F $ x / y) (checkUnits u u')
evalOp Mod [Number (I x) u, Number (I y) u']  = Number (I $ mod x y) (checkUnits u u')

evalOp Not [Boolean b]            = Boolean (not b)
evalOp And [Boolean x, Boolean y] = Boolean (x && y) 
evalOp Or  [Boolean x, Boolean y] = Boolean (x || y)

evalOp Eq  [Number (I x) u, Number (I y) u']  = Boolean (x == y)
evalOp Eq  [Number (F x) u, Number (F y) u']  = Boolean (x == y)
evalOp Ne  [Number (I x) u, Number (I y) u']  = Boolean (not $ x == y)
evalOp Ne  [Number (F x) u, Number (F y) u']  = Boolean (not $ x == y)
evalOp Gt  [Number (I x) u, Number (I y) u']  = Boolean (x > y)
evalOp Gt  [Number (F x) u, Number (F y) u']  = Boolean (x > y)
evalOp Lt  [Number (I x) u, Number (I y) u']  = Boolean (x < y)
evalOp Lt  [Number (F x) u, Number (F y) u']  = Boolean (x < y)
evalOp Ge  [Number (I x) u, Number (I y) u']  = Boolean (x >= y)
evalOp Ge  [Number (F x) u, Number (F y) u']  = Boolean (x >= y)
evalOp Le  [Number (I x) u, Number (I y) u']  = Boolean (x <= y)
evalOp Le  [Number (F x) u, Number (F y) u']  = Boolean (x <= y)

evalOp ListCons [Listy Empty, v]    = Listy (Cons v Empty)
evalOp ListCons [Listy l,     v]    = Listy (Cons v l) 
evalOp Head     [Listy (Cons v _)]  = v
evalOp Tail     [Listy (Cons _ l)]  = Listy l
evalOp IsEmpty  [Listy Empty]       = Boolean True
evalOp IsEmpty  _                   = Boolean False

evalOp op vs = P op vs

checkUnits :: Maybe Unit -> Maybe Unit -> Maybe Unit 
checkUnits x y = if x == y then x else error $ "Conflicting units: " ++ (show x) ++ " =/= " ++ (show y)