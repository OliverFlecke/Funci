-- This module will be able to transform the tokens from the lexer 
-- into an abstract syntax tree (AST), which is the internal 
-- representation of the program
module HandParser (
  parseString,
  parse,
  parseArithmicString,
  parseBooleanString) where 

import HandSyntax
import HandLexer

parseString :: String -> Either String Program
parseString s = lexer s >>= parse

parse :: [Token] -> Either String Program
parse tokens = fst `fmap` parseLExpr tokens

parseLExpr :: [Token] -> Either String (Expr, [Token])
parseLExpr (Keyword Let : Identifier x : Operator Assignment : rest) =
  case parseSExpr rest of 
    Left s                          -> Left s
    Right (e1, Keyword In : rest')  -> do 
      (e2, rest'') <- parseSExpr rest' 
      return $ (LetIn x e1 e2, rest'')
    _                               -> Left $ "Parser error: Expecting 'in' in let expression: " ++ (show rest)
parseLExpr (Keyword If : rest) = 
  case parseBExpr rest of 
    Left s        -> Left s
    Right (b, Keyword Then : rest') -> do 
      return $ (IfThenElse b (Const 0) (Const 0), rest') -- Not working !!!
    _                               -> Left $ "Parser error: If then else something"

parseLExpr tokens = parseSExpr tokens


-- The B parser is used for parsing boolean expressions
-- parseBoolean and parseBooleanString are helper functions to access the sub-language
parseBoolean :: [Token] -> Either String Program
parseBoolean tokens = fst `fmap` parseBExpr tokens

parseBooleanString :: String -> Either String Program
parseBooleanString s = lexer s >>= parseBoolean

parseBCExpr :: [Token] -> Either String (Expr, [Token])
parseBCExpr (Boolean True : rest) = return (ConstBool True, rest)
parseBCExpr (Boolean False : rest) = return (ConstBool False, rest)
parseBCExpr (Identifier x : rest) = return (Var x, rest)
parseBCExpr (Operator Not : rest) = do 
  (b, rest') <- parseBCExpr rest
  return (App (Prim Not) b, rest')
parseBCExpr (Bracket LeftParen : rest) = 
  case parseBExpr rest of 
    Right (expr, Bracket RightParen : rest')  -> return (expr, rest')
    Right (expr, rest')                       -> Left $ "Parser error: Expected `)` at " ++ (show rest)
    Left s                                    -> Left s                  
parseBCExpr rest = Left $ "Parser error: Expecting an boolean constant or expression: " ++ (show rest)

parseBAExpr :: [Token] -> Either String (Expr, [Token])
parseBAExpr tokens = do
  ((e : exprs), rest) <- parseBAExpr' tokens  
  return (foldl (applyOperator And) e exprs, rest) 
  where 
    parseBAExpr' :: [Token] -> Either String ([Expr], [Token]) 
    parseBAExpr' tokens = do
      case parseBCExpr tokens of 
        Right (expr, Operator And : rest')  -> do 
          (exprs, rest'') <- parseBAExpr' rest'
          return (expr : exprs, rest'')
        Right (expr, rest')                 -> Right ([expr], rest')
        Left s                              -> Left s

parseBExpr :: [Token] -> Either String (Expr, [Token])
parseBExpr tokens = do
  ((e : exprs), rest) <- parseBExpr' tokens  
  return (foldl (applyOperator Or) e exprs, rest) 
  where 
    parseBExpr' :: [Token] -> Either String ([Expr], [Token]) 
    parseBExpr' tokens = do
      case parseBAExpr tokens of 
        Right (expr, Operator Or : rest')   -> do 
          (exprs, rest'') <- parseBExpr' rest'
          return (expr : exprs, rest'')
        Right (expr, rest')                 -> Right ([expr], rest')
        Left s                              -> Left s



-- Parsing the arithmic language 
-- Parser functions to only parse arithmics
parseArithmicString :: String -> Either String Program
parseArithmicString s = lexer s >>= parseArithmic 

parseArithmic :: [Token] -> Either String Program
parseArithmic tokens = fst `fmap` parseSExpr tokens

-- The F, P, and S expression are used to handle the basic alrithmic operations
-- F expressions
parseFExpr :: [Token] -> Either String (Expr, [Token])
parseFExpr (Num i : rest) = return (Const i, rest)
parseFExpr (Identifier x : rest) = return (Var x, rest)
parseFExpr (Bracket RightParen : rest) = 
  case parseSExpr rest of 
    Right (expr, Bracket LeftParen : rest') -> return (expr, rest')
    Right (expr, rest')                     -> Left $ "Parse error: Expected `)` at " ++ (show rest)
    Left s                                  -> Left s
parseFExpr tokens = Left $ "Parse error: Expecting a number or an `(` at " ++ (show tokens)

-- P expressions
parsePExpr :: [Token] -> Either String (Expr, [Token])
parsePExpr tokens = do 
  ((e : exprs), rest, ops) <- parsePExpr' tokens
  return (foldlWfs (createOpExprFuncs ops) e exprs, rest)
  where 
    parsePExpr' :: [Token] -> Either String ([Expr], [Token], [Operator])
    parsePExpr' token = 
      case parseFExpr token of 
        Right (expr, Operator Mul : rest')  -> parsePExpr' rest' >>= concatExprUsingOp expr Mul
        Right (expr, Operator Div : rest')  -> do
          (exprs, rt, ops) <- parsePExpr' rest' 
          case exprs of -- Handling divide by zero
            (Const 0:xs)  -> Left "Arithmic error: Divide by zero"
            _             -> return (expr : exprs, rt, Div : ops)
        Right (expr', rest')                -> return ([expr'], rest', [Mul])
        Left s                              -> Left s

-- S expressions
parseSExpr :: [Token] -> Either String (Expr, [Token])
parseSExpr tokens = do
  ((e : exprs), rest, ops) <- parseSExpr' tokens
  Right (foldlWfs (createOpExprFuncs ops) e exprs, rest)
  where 
    parseSExpr' :: [Token] -> Either String ([Expr], [Token], [Operator])
    parseSExpr' token = 
      case parsePExpr token of 
        Right (expr, Operator Add : rest')  -> parseSExpr' rest' >>= concatExprUsingOp expr Add
        Right (expr, Operator Sub : rest')  -> parseSExpr' rest' >>= concatExprUsingOp expr Sub
        Right (expr', rest')                -> return ([expr'], rest', [Add]) 
        Left s                              -> Left s

applyOperator :: Operator -> (Expr -> Expr -> Expr)
applyOperator op = (\x -> (App (App (Prim op) x)))

-- Create a list of functions to apply operators 
createOpExprFuncs :: [Operator] -> [(Expr -> Expr -> Expr)]
createOpExprFuncs [] = []
createOpExprFuncs (op:ops) = applyOperator op : createOpExprFuncs ops
 
-- Might need a better name, but this is what it does
concatExprUsingOp :: Expr -> Operator -> (([Expr], [Token], [Operator]) -> Either String ([Expr], [Token], [Operator]))
concatExprUsingOp expr op = (\(exprs, rt, ops) -> return (expr : exprs, rt, op : ops))

-- Alternative version of foldl which can fold with multible functions
foldlWfs :: [(Expr -> Expr -> Expr)] -> Expr -> [Expr] -> Expr
foldlWfs _ z [] = z 
foldlWfs [] _ _ = error $ "This should not happen!"
foldlWfs (f:fs) z (x:xs) = foldlWfs fs (f z x) xs