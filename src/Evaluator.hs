module Evaluator (
  evaluateString,
  evaluateStringWithEnv,
  evaluate
  ) where

import Syntax
import Lexer
import Parser
import qualified Environment as E

evaluateString :: (Read a, Show a, Ord a, Num a, RealFrac a) => String -> Either (Exception a) (Value a)
evaluateString s = do
    ts <- lexer s
    p <- parse ts
    evaluate p

evaluateStringWithEnv :: (Read a, Show a, Ord a, Num a, RealFrac a) => VEnv a -> String -> Either (Exception a) (Value a)
evaluateStringWithEnv g s = do 
  ts <- lexer s
  case parseExpressions ts of 
    Left e        -> Left e 
    Right (p, []) -> evalE g p -- This is on purpuse for the interactive
    Right (p, t)  -> Left $ EvaluatorError $ "There should be now more tokens:\nProgram " ++ (show p) ++ "\ntokens:" ++ (show t)

evaluate :: (Read a, Show a, Ord a, Num a, RealFrac a) => Program a -> Either (Exception a) (Value a)
evaluate p = evalP E.empty p

evalP :: (Read a, Show a, Ord a, Num a, RealFrac a) => VEnv a -> Program a -> Either (Exception a) (Value a)
evalP g [] =
  case E.lookup g "main" of
    Just (Fun f g' [] e)  -> evalE g e
    Nothing               -> Left $ ScopeError "main" g
evalP g (Bind f _ vs e : rest) =
  let g' = E.add g (f, Fun f E.empty vs e)
  in evalP g' rest

-- Evaluate an expression
evalE :: (Read a, Show a, Ord a, Num a, RealFrac a) => VEnv a -> Expr a -> Either (Exception a) (Value a)
evalE g (Const n) = return n
evalE g (Var x)   =
  case E.lookup g x of
    Just (Fun f g' [] e)  -> evalE (E.union g' g) e
    Just (Fun f g' vs e)  -> return $ Fun f (E.union g' g) vs e
    Just n                -> return n
    Nothing -> Left $ ScopeError x g

evalE g (LetIn ((Bind x _ ids e):[]) End) = return $ Fun x g ids e
evalE g (LetIn ((Bind x _ ids e):[]) b) = let g' = E.add g (x, Fun x g ids e)
                                in evalE g' b
evalE g (LetIn ((Bind x _ ids e):xs) b) = let g' = E.add g (x, Fun x g ids e)
                                in evalE g' (LetIn xs b)

evalE g (IfThenElse b t f) = do
  b' <- evalE g b
  case b' of
    Boolean True  -> evalE g t
    Boolean False -> evalE g f
    _             -> Left $ EvaluatorError $ (show b) ++ " should have type bool"

-- Applying operators
evalE g (Prim op) = return $ P op []
evalE g (App (App (Prim Sub) x) y) = do
  ex <- evalE g x
  ey <- evalE g y
  case (ex, ey) of
    (Number x' u, Number y' u')  -> checkUnits u u' >>= applyOp (-) x' y'

evalE g (App a b) = case evalE g a of
  Right (P op v)          -> do
    b' <- evalE g b
    evalOp op (v ++ [b'])
  Right (C id v)          -> do
    b' <- evalE g b
    return $ C id (v ++ [b'])
  Right (Fun _ g' (v:[]) e) -> do
    b' <- evalE g b
    let g'' = E.add g' (v, b')
    evalE g'' e
  Right (Fun f g' (v:vs) e) -> do
    b' <- evalE g b
    let g'' = E.add g' (v, b')
    return $ Fun f g'' vs e
  Left s                  -> Left s
  t                       -> Left $ EvaluatorError $ "Could not be evaluated: " ++ (show t) ++ "\n" ++ (show a) ++ "\n" ++ (show b)

evalE g e = Left $ EvaluatorError $ show e

applyOp :: Num a => (a -> a -> a) -> a -> a -> (a, a, Unit) -> Either (Exception a) (Value a)
applyOp op x y = (\(m, n, unit) -> return $ Number ((m * x) `op` (n * y)) unit)

-- Evaluate operators
evalOp :: (Read a, Show a, Num a, Ord a, RealFrac a) => Operator -> [Value a] -> Either (Exception a) (Value a)
evalOp Sub [Number x u]               = return $ Number (-x) u
evalOp Add [Number x u, Number y u']  = checkUnits u u' >>= applyOp (+) x y
evalOp Sub [Number x u, Number y u']  = checkUnits u u' >>= applyOp (-) x y
evalOp Mul [Number x u, Number y u']  = checkUnitMD (+) u u' >>= applyOp (*) x y
evalOp Div [_         , Number 0 _ ]  = Left $ DivideByZero
evalOp Div [Number x u, Number y u']  = checkUnitMD (-) u u' >>= applyOp (/) x y
-- Really find a better way to include the mod operator
evalOp Mod [Number x u, Number y u']  = do
  (m, n, unit) <- checkUnits u u'
  return $ Number (fromIntegral $ mod (floor $ m * x) (floor $ n * y)) unit

evalOp Not [Boolean b]            = return $ Boolean (not b)
evalOp And [Boolean x, Boolean y] = return $ Boolean (x && y)
evalOp Or  [Boolean x, Boolean y] = return $ Boolean (x || y)

evalOp Eq  [Number x u, Number y u']  = return $ Boolean (x == y)
evalOp Ne  [Number x u, Number y u']  = return $ Boolean (not $ x == y)
evalOp Gt  [Number x u, Number y u']  = return $ Boolean (x > y)
evalOp Lt  [Number x u, Number y u']  = return $ Boolean (x < y)
evalOp Ge  [Number x u, Number y u']  = return $ Boolean (x >= y)
evalOp Le  [Number x u, Number y u']  = return $ Boolean (x <= y)

evalOp ListCons [Listy Empty, v]    = return $ Listy (Cons v Empty)
evalOp ListCons [Listy l,     v]    = return $ Listy (Cons v l)
evalOp Head     [Listy (Cons v _)]  = return $ v
evalOp Tail     [Listy (Cons _ l)]  = return $ Listy l
evalOp IsEmpty  [Listy Empty]       = return $ Boolean True
evalOp IsEmpty  _                   = return $ Boolean False

evalOp op vs = return $ P op vs

checkUnits :: Fractional a => Unit -> Unit -> Either (Exception a) (a, a, Unit)
checkUnits (Unit []) (Unit []) = return (1, 1, Unit [])
checkUnits (Unit ((u, p, e):us)) (Unit ((u', p', e'):us')) =
  if u == u' && e == e'
    then do
      (m, n, Unit rest) <- checkUnits (Unit us) (Unit us')
      let (m', n', pNew) = newPrefix p p'
      return (m * m', n * n', Unit ((u, pNew, e):rest))
    else Left $ InvalidUnits u u'

checkUnitMD :: Fractional a => (Exponent -> Exponent -> Exponent) -> Unit -> Unit -> Either (Exception a) (a, a, Unit)
checkUnitMD f (Unit []) (Unit []) = return (1, 1, Unit [])
checkUnitMD f (Unit ((u, p, e):us)) (Unit ((u', p', e'):us')) =
  if u == u'
    then do
      out@(m, n, Unit rest) <- checkUnitMD f (Unit us) (Unit us')
      if f e e' == 0
        then return out
        else let (m', n', pNew) = newPrefix p p'
              in return (m' * m, n' * n, Unit ((u, pNew, f e e') : rest))
    else Left $ InvalidUnits u u'

newPrefix :: (Num a, Fractional a) => UnitPrefix -> UnitPrefix -> (a, a, UnitPrefix)
newPrefix x y = let m = prefixValue x
                    n = prefixValue y
                    diff = 10^^(m - n)
                in if x < y
                    then (1, diff, x)
                    else (diff, 1, y)

-- The values which the prefixes corrispond to
prefixValue :: Num a => UnitPrefix -> a
prefixValue Yotta = 24
prefixValue Zetta = 21
prefixValue Exa   = 18
prefixValue Peta  = 15
prefixValue Tera  = 12
prefixValue Giga  = 9
prefixValue Mega  = 6
prefixValue Kilo  = 3
prefixValue Hecto = 2
prefixValue Deca  = 1
prefixValue None  = 0
prefixValue Deci  = (-1)
prefixValue Centi = (-2)
prefixValue Milli = (-3)
prefixValue Micro = (-6)
prefixValue Nano  = (-9)
prefixValue Pico  = (-12)
prefixValue Femto = (-15)
prefixValue Atto  = (-18)
prefixValue Zepto = (-21)
prefixValue Yocto = (-24)
