-- This module will be able to transform the tokens from the lexer
-- into an abstract syntax tree (AST), which is the internal
-- representation of the program
module Parser (
  parseString,
  parse,
  parseArithmicString,
  parseExpressionsString) where

import Syntax
import Lexer
import Data.Text (strip, pack, unpack)
import Data.List.Split
import Text.Regex.Posix

parseString :: String -> Either String Program
parseString s = lexer s >>= parse

parse :: [Token] -> Either String Program
parse [] = Right []
parse (Semicolon : rest)  = parse rest
parse (Identifier id : rest) = do
  (bind, rest') <- parseBind id rest
  p <- parse rest'
  return $ bind : p
parse tokens              = error (show tokens)

-- Parse bind expressions
parseBind :: String -> [Token] -> Either String (Bind, [Token])
parseBind id rest =
  let (rest', vs, ty) = findParmsAndType rest []
  in
    case parseExpressions rest' of
      Right (n, r)  -> return $ (Bind id ty vs n, r)
      Left s        -> Left s
  where
    findParmsAndType :: [Token] -> [Id] -> ([Token], [Id], Maybe QType)
    findParmsAndType (Operator Assignment : rest) acc     = (rest, acc, Nothing)
    findParmsAndType (Identifier id : rest) acc           = findParmsAndType rest (acc ++ [id])
    findParmsAndType (Operator TypeAssignment : rest) acc =
      case parseType rest of
        (Operator Assignment : rest', ty) -> (rest', acc, ty)
        (rest', ty)                       -> error $ "Rest: " ++ (show rest') ++ "\nType: " ++ (show ty)
    findParmsAndType rest acc                             = (rest, acc, Nothing)
    parseType :: [Token] -> ([Token], Maybe QType)
    parseType (BType ty : Operator TypeArrow : tokens)  =
      let (outTokens, Just (Ty ty')) = parseType tokens
      in (outTokens, Just (Ty (Arrow (Base ty) ty')))
    parseType (Bracket LeftParen : tokens) =
      case parseType tokens of
        (Bracket RightParen : Operator TypeArrow : rest, Just (Ty ty)) ->
          let (outTokens, Just (Ty ty')) = parseType rest
          in (outTokens, Just (Ty (Arrow ty ty')))
        (Bracket RightParen : rest', ty)                      -> (rest', ty)
        _                                                     -> error $ "Invalid type signature"
    parseType (BType t : rest)                        = (rest, Just (Ty (Base t)))
    parseType tokens                                  = (tokens, Nothing)

-- Parse let expression
parseExpressionsString :: String -> Either String Expr
parseExpressionsString s = lexer s >>= (\t -> fst `fmap` parseExpressions t)

parseExpressions :: [Token] -> Either String (Expr, [Token])
parseExpressions (Keyword Let : rest) = parseLet rest
  where
    findArguments (Identifier x : rest) ids        = findArguments rest (ids ++ [x])
    findArguments (Operator Assignment : rest) ids = (ids, rest)
    parseLet :: [Token] -> Either String (Expr, [Token])
    parseLet (Identifier x : rest) = do
      (bind, rest') <- parseBind x rest
      case rest' of
        Keyword In : rest'' -> do
          (body, r) <- parseExpressions rest''
          return $ (LetIn [bind] body, r)
        Operator Comma : rest'' -> do
          (LetIn xs body, r) <- parseLet rest''
          return $ (LetIn (bind : xs) body, r)
        rest''              -> error $ "\nBefore: " ++ (show rest) ++ "\n\nAfter:  " ++ (show rest'') ++ "\nBind: " ++ (show bind)

-- Parsing if expressions
parseExpressions (Keyword If : rest) =
  case parseArithmic rest of
    Left s                          -> Left s
    Right (b, Keyword Then : rest') ->
      case parseExpressions rest' of
        Left s                            -> Left s
        Right (e1, Keyword Else : rest'') -> do
          (e2, r) <- parseExpressions rest''
          return $ (IfThenElse b e1 e2, r)
        Right (_, rest'')                 -> Left $ "Parser error: Expected keyword 'else' at " ++ (show rest'')
    Right (_, rest')                -> Left $ "Parser error: Expected keyword 'then' at " ++ (show rest')

parseExpressions tokens = parseArithmic tokens

-- Parsing the arithmic language
-- Parser functions to only parse arithmics
parseArithmicString :: String -> Either String Expr
parseArithmicString s = lexer s >>= (\t -> fst `fmap` parseArithmic t)

parseArithmic :: [Token] -> Either String (Expr, [Token])
parseArithmic tokens = parse15Expr tokens

-- Parsing basics like numbers and boolean
parseBase :: [Token] -> Either String (Expr, [Token])
parseBase (Semicolon : rest)            = return (End, rest)
-- Parsing units on numbers
parseBase (Num n : Units s : rest)      = return (Const (Number n (parseUnit s)), rest)
parseBase (Num n : rest)                = return (Const (Number n (Unit [])), rest)
parseBase (Booly b : rest)              = return (Const (Boolean b), rest)
parseBase (Identifier x : rest) = applyArgument (Var x) rest
  where
    applyArgument :: Expr -> [Token] -> Either String (Expr, [Token])
    applyArgument a (Operator Sub : rest) = return (a, Operator Sub : rest)
    applyArgument a t =
      case parseArithmic t of
        Right (End, rest)     -> return (a, rest)
        Right (b, rest)       -> applyArgument (App a b) rest
        _                     -> return (a, t)
parseBase t = Left $ "Parse error: Expecting a number or an `(` at " ++ (show t)

-- Operators with precedence level 1
parse1Expr :: [Token] -> Either String (Expr, [Token])
parse1Expr (Operator Not : rest)      = applyUnaryOp rest Not
parse1Expr (Operator Sub : rest)      = applyUnaryOp rest Sub
parse1Expr (Operator Head : rest)     = applyUnaryOp rest Head
parse1Expr (Operator Tail : rest)     = applyUnaryOp rest Tail
parse1Expr (Operator IsEmpty : rest)  = applyUnaryOp rest IsEmpty
parse1Expr (Bracket LeftParen : rest) =
  case parseExpressions rest of
    Right (expr, Bracket RightParen : rest')  -> return (expr, rest')
    Right (expr, rest')                       -> Left $ "Parser error: Expected `)` at " ++ (show rest)
    Left s                                    -> Left s
parse1Expr (Bracket LeftSquareBracket : Bracket RightSquareBracket : rest) = return (Const (Listy Empty), rest)
-- parse1Expr (Bracket LeftSquareBracket : rest) =
--   case parse15Expr rest of
--     Right (expr, Bracket RightSquareBracket : rest')  -> return (App (App (Prim ListCons) expr) (Const (Listy Empty)), rest')
--     Right (expr, rest')                               -> Left $ "Parser error: Expected `)` at " ++ (show rest)
--     Left s                                            -> Left s
parse1Expr t = parseBase t

parse2Expr :: [Token] -> Either String (Expr, [Token])
parse2Expr tokens = parse1Expr tokens

parse3Expr :: [Token] -> Either String (Expr, [Token])
parse3Expr t = checkOps [Mul, Div, Mod] parse2Expr t >>= foldOperators

parse4Expr :: [Token] -> Either String (Expr, [Token])
parse4Expr t = checkOps [Add, Sub] parse3Expr t >>= foldOperators

parse6Expr :: [Token] -> Either String (Expr, [Token])
parse6Expr t = checkOps [Gt, Lt, Ge, Le] parse4Expr t >>= foldOperators

parse7Expr :: [Token] -> Either String (Expr, [Token])
parse7Expr t = checkOps [Eq, Ne] parse6Expr t >>= foldOperators

parse11Expr :: [Token] -> Either String (Expr, [Token])
parse11Expr t = checkOps [And] parse7Expr t >>= foldOperators

parse12Expr :: [Token] -> Either String (Expr, [Token])
parse12Expr t = checkOps [Or] parse11Expr t >>= foldOperators

parse15Expr :: [Token] -> Either String (Expr, [Token])
parse15Expr t = do
  (exprs, r, ops) <- checkOps [ListCons] parse12Expr t
  foldOperators (reverse exprs, r, ops)

-- Helper functions
checkOps :: [Operator] -> ([Token] -> Either String (Expr, [Token])) -> [Token] -> Either String ([Expr], [Token], [Operator])
checkOps ops f t =
  case f t of
    Right (expr, Operator op : rest)  ->
        if elem op ops
          then checkOps ops f rest >>= concatExprUsingOp expr op
          else return ([expr], Operator op : rest, [])
    Right (expr, rest)                -> return ([expr], rest, [])
    Left s                            -> Left s

applyOperator :: Operator -> (Expr -> Expr -> Expr)
applyOperator op = (\x -> (App (App (Prim op) x)))

applyUnaryOp :: [Token] -> Operator -> Either String (Expr, [Token])
applyUnaryOp rest op = parse1Expr rest >>= (\(e, rest') -> return (App (Prim op) e, rest'))

-- Create a list of functions to apply operators
createOpExprFuncs :: [Operator] -> [(Expr -> Expr -> Expr)]
createOpExprFuncs [] = []
createOpExprFuncs (op:ops) = applyOperator op : createOpExprFuncs ops

-- Might need a better name, but this is what it does
concatExprUsingOp :: Expr -> Operator -> (([Expr], [Token], [Operator]) -> Either String ([Expr], [Token], [Operator]))
concatExprUsingOp expr op = (\(exprs, rt, ops) -> return (expr : exprs, rt, op : ops))

foldOperators :: ([Expr], [Token], [Operator]) -> Either String (Expr, [Token])
foldOperators = (\((e : exprs), rest, ops) -> return $ (foldlWfs (createOpExprFuncs ops) e exprs, rest))
  where
    -- Alternative version of foldl which can fold with multible functions
    foldlWfs :: [(Expr -> Expr -> Expr)] -> Expr -> [Expr] -> Expr
    foldlWfs _ z [] = z
    foldlWfs [] _ _ = error $ "This should not happen!"
    foldlWfs (f:fs) z (x:xs) = foldlWfs fs (f z x) xs

-- Parsing units
parseUnit :: String -> Unit
parseUnit s =
  let units = splitOn "*" s
  in go units
  -- in error $ show units
  where
    go :: [String] -> Unit 
    go []     = Unit []
    go (x:xs) =
      let (pu:rest) = splitOn "^" (unpack (strip (pack x)))
          Unit us = go xs
      in Unit ((findPrefix pu, findUnit pu, findExpo rest) : us)
    findUnit :: String -> BaseUnit
    findUnit s
      | s =~ "m$"   = Metre
      | s =~ "s$"   = Second
      | s =~ "g$"   = Gram
      | s =~ "A$"   = Ampere
      | s =~ "K$"   = Kelvin
      | s =~ "mol$" = Mole
      | s =~ "cd$"  = Candela
      | otherwise   = CustomUnit s
    findPrefix :: String -> UnitPrefix
    findPrefix "cd"   = None
    findPrefix "mol"  = None
    findPrefix s
      | s =~ "^Y[a-zA-Z]+"   = Yotta
      | s =~ "^Z[a-zA-Z]+"   = Zetta
      | s =~ "^E[a-zA-Z]+"   = Exa
      | s =~ "^P[a-zA-Z]+"   = Peta
      | s =~ "^T[a-zA-Z]+"   = Tera
      | s =~ "^G[a-zA-Z]+"   = Giga
      | s =~ "^M[a-zA-Z]+"   = Mega
      | s =~ "^k[a-zA-Z]+"   = Kilo
      | s =~ "^h[a-zA-Z]+"   = Hecto
      | s =~ "^da[a-zA-Z]+"  = Deca
      | s =~ "^d[a-zA-Z]+"   = Deci
      | s =~ "^c[a-zA-Z]+"   = Centi
      | s =~ "^mu[a-zA-Z]+"  = Micro -- This and the next line are switched, as the next will make this redundant and wrong
      | s =~ "^m[a-zA-Z]+"   = Milli
      | s =~ "^n[a-zA-Z]+"   = Nano
      | s =~ "^p[a-zA-Z]+"   = Pico
      | s =~ "^f[a-zA-Z]+"   = Femto
      | s =~ "^a[a-zA-Z]+"   = Atto
      | s =~ "^z[a-zA-Z]+"   = Zepto
      | s =~ "^y[a-zA-Z]+"   = Yocto
      | otherwise   = None
    findExpo :: [String] -> Exponent
    findExpo []     = 1
    findExpo (n:[]) = read n