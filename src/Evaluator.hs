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
    (Number (I x') u, Number (I y') u')  -> let (m, n, unit) = checkUnits u u' in Number (I ((m * x') - (n * y'))) unit
    (Number (F x') u, Number (F y') u')  -> let (m, n, unit) = checkUnits u u' in Number (F ((x') - (y'))) unit
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
evalOp Add [Number (I x) u, Number (I y) u']  = let (m, n, unit) = checkUnits u u' in Number (I $ (m * x) + (n * y)) unit
evalOp Add [Number (F x) u, Number (F y) u']  = let (m, n, unit) = checkUnits u u' in Number (F $ (x) + (y)) unit
evalOp Sub [Number (I x) u, Number (I y) u']  = let (m, n, unit) = checkUnits u u' in Number (I $ (m * x) - (n * y)) unit
evalOp Sub [Number (F x) u, Number (F y) u']  = let (m, n, unit) = checkUnits u u' in Number (F $ (x) - (y)) unit
evalOp Mul [Number (I x) u, Number (I y) u']  = let (m, n, unit) = checkUnitMD (+) u u' in Number (I $ (m * x) * (n * y)) unit
evalOp Mul [Number (F x) u, Number (F y) u']  = let (m, n, unit) = checkUnitMD (+) u u' in Number (F $ (x) * (y)) unit
evalOp Div [Number (I x) u, Number (I y) u']  = let (m, n, unit) = checkUnitMD (-) u u' in Number (I $ quot (m * x) (n * y)) unit
evalOp Div [Number (F x) u, Number (F y) u']  = let (m, n, unit) = checkUnitMD (-) u u' in Number (F $ (x) / (y)) unit
evalOp Mod [Number (I x) u, Number (I y) u']  = let (m, n, unit) = checkUnits u u' in Number (I $ mod (m * x) (n * y)) unit

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

checkUnits :: Unit -> Unit -> (Int, Int, Unit)
checkUnits (Unit ((u, p, e):us)) (Unit ((u', p', e'):us')) =
  if u == u' && e == e'
    then let (m, n, Unit rest) = checkUnits (Unit us) (Unit us')
             (m', n', pNew) = findPrefixDif p p'
          in (m * m', n * n', Unit ((u, pNew, e):rest))
    else error $ "Conflicting units: " ++ (show u) ++ " =/= " ++ (show u')
checkUnits x y = if x == y then (1, 1, x) else error $ "Conflicting units: " ++ (show x) ++ " =/= " ++ (show y)

checkUnitMD :: (Exponent -> Exponent -> Exponent) -> Unit -> Unit -> (Int, Int, Unit)
checkUnitMD f (Unit []) (Unit []) = (1, 1, Unit [])
checkUnitMD f (Unit ((u, p, e):us)) (Unit ((u', p', e'):us')) =
  if u == u'
    then let out@(m, n, Unit rest) = checkUnitMD f (Unit us) (Unit us')
          in if f e e' == 0
            then out
            else let (m', n', pNew) = findPrefixDif p p'
                  in (m' * m, n' * n, Unit ((u, pNew, e + e') : rest))
    else error $ "Conflicting units: " ++ (show u) ++ " =/= " ++ (show u')

findPrefixDif :: UnitPrefix -> UnitPrefix -> (Int, Int, UnitPrefix)
findPrefixDif x y = let m = prefixValue x
                        n = prefixValue y
                    in if x > y
                      then (m, n, y)
                      else (m, n, x)

-- The values which the prefixes corrispond to
prefixValue :: UnitPrefix -> Int
prefixValue None  = 1
prefixValue Yotta = 10^24
prefixValue Zetta = 10^21
prefixValue Exa   = 10^18
prefixValue Peta  = 10^15
prefixValue Tera  = 10^12
prefixValue Giga  = 10^9
prefixValue Mega  = 10^6
prefixValue Kilo  = 10^3
prefixValue Hecto = 10^2
prefixValue Deca  = 10
prefixValue Deci  = 10^(-1)
prefixValue Centi = 10^(-2)
prefixValue Milli = 10^(-3)
prefixValue Micro = 10^(-6)
prefixValue Nano  = 10^(-9)
prefixValue Pico  = 10^(-12)
prefixValue Femto = 10^(-15)
prefixValue Atto  = 10^(-18)
prefixValue Zepto = 10^(-21)
prefixValue Yocto = 10^(-24)