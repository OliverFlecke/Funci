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

parseString :: String -> Either String Program
parseString s = lexer s >>= parse

parse :: [Token] -> Either String Program
parse [] = Right []
parse (Identifier id : Operator Assignment : rest) = 
  case parseExpressions rest of 
    Right (n, r)  -> do 
      p <- parse r 
      return $ Bind id Nothing [] n : p
    _ -> Left $ "Parser error: Could not parse the function definition"

parse tokens = undefined

-- Parse let expression
parseExpressionsString :: String -> Either String Expr 
parseExpressionsString s = lexer s >>= (\t -> fst `fmap` parseExpressions t)

parseExpressions :: [Token] -> Either String (Expr, [Token])
parseExpressions (Keyword Let : rest) = parseLet rest
  where 
    parseLet :: [Token] -> Either String (Expr, [Token])
    parseLet (Identifier x : Operator Assignment : rest) = 
      case parseArithmic rest of 
        Right (e, Keyword In : rest')     -> do 
          (body, rest'') <- parseArithmic rest' 
          return $ (LetIn [(x, e)] body, rest'')
        Right (e, Operator Comma : rest') -> do 
          (LetIn xs body, rest'') <- parseLet rest' 
          return $ (LetIn ((x, e):xs) body, rest)
        Left s                            -> Left s
        _                                 -> error "Let error"

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

checkOps :: [Operator] -> ([Token] -> Either String (Expr, [Token])) -> [Token] -> Either String ([Expr], [Token], [Operator])
checkOps ops f t =
  case f t of 
    Right (expr, Operator op : rest)  -> 
        if elem op ops 
          then checkOps ops f rest >>= concatExprUsingOp expr op
          else return ([expr], Operator op : rest, [])
    Right (expr, rest)                -> return ([expr], rest, []) 
    Left s                            -> Left s                                  

-- Parsing basics like numbers and boolean
parseBase :: [Token] -> Either String (Expr, [Token])
parseBase (Num n : rest)              = return (Const (Number n), rest)
parseBase (Booly b : rest)          = return (Const (Boolean b), rest)
parseBase (Identifier x : rest)       = return (Var x, rest)
parseBase t = Left $ "Parse error: Expecting a number or an `(` at " ++ (show t)

-- Operators with precedence level 1
parse1Expr :: [Token] -> Either String (Expr, [Token])
parse1Expr (Operator Not : rest) = parse1Expr rest >>= (\(b, rest') -> return (App (Prim Not) b, rest'))
parse1Expr (Bracket LeftParen : rest) = 
  case parse15Expr rest of 
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
applyOperator :: Operator -> (Expr -> Expr -> Expr)
applyOperator op = (\x -> (App (App (Prim op) x)))

applyOperator2 :: Operator -> (Expr -> Expr -> Expr)
applyOperator2 op = (\x y -> (App (App (Prim op) y) x))

-- Create a list of functions to apply operators 
createOpExprFuncs :: [Operator] -> [(Expr -> Expr -> Expr)]
createOpExprFuncs [] = []
createOpExprFuncs (op:ops) = applyOperator op : createOpExprFuncs ops
 
-- Might need a better name, but this is what it does
concatExprUsingOp :: Expr -> Operator -> (([Expr], [Token], [Operator]) -> Either String ([Expr], [Token], [Operator]))
concatExprUsingOp expr op = (\(exprs, rt, ops) -> return (expr : exprs, rt, op : ops))

foldOperators :: ([Expr], [Token], [Operator]) -> Either String (Expr, [Token])
foldOperators = (\((e : exprs), rest, ops) -> return $ (foldlWfs (createOpExprFuncs ops) e exprs, rest))

-- Alternative version of foldl which can fold with multible functions
foldlWfs :: [(Expr -> Expr -> Expr)] -> Expr -> [Expr] -> Expr
foldlWfs _ z [] = z 
foldlWfs [] _ _ = error $ "This should not happen!"
foldlWfs (f:fs) z (x:xs) = foldlWfs fs (f z x) xs

foldrWfs :: [(Expr -> Expr -> Expr)] -> Expr -> [Expr] -> Expr
foldrWfs _ z [] = z 
-- foldrWfs [] _ _ = error $ 
foldrWfs (f:fs) z (x:xs) = f x (foldrWfs fs z xs)