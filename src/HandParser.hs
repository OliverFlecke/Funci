-- This module will be able to transform the tokens from the lexer 
-- into an abstract syntax tree (AST), which is the internal 
-- representation of the program
module HandParser (
  parseString,
  parse,
  parseArithmicString,
  parseBooleanString,
  parseListString,
  parseLetString) where 

import HandSyntax
import HandLexer

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
parseLetString :: String -> Either String Expr 
parseLetString s = lexer s >>= (\t -> fst `fmap` parseExpressions t)

parseExpressions :: [Token] -> Either String (Expr, [Token])
parseExpressions (Keyword Let : Identifier x : Operator Assignment : rest) =
  case parseBasic rest of 
    Right (e1, Keyword In : rest')  -> do 
      (e2, rest'') <- parseBasic rest' 
      return $ (LetIn x e1 e2, rest'')
    _                               -> Left $ "Parser error: Expecting 'in' in let expression: " ++ (show rest)
parseExpressions (Keyword If : rest) = 
  case parseBExpr rest of 
    Left s        -> Left s
    Right (b, Keyword Then : rest') -> do 
      return $ (IfThenElse b (Const (Number (Integer 0))) (Const (Number (Integer 0))), rest') -- Not working !!!
    _                               -> Left $ "Parser error: If then else something"

parseExpressions tokens = parseBasic tokens

-- Handling lists
parseListString :: String -> Either String Expr
parseListString s = do 
  (e, rest) <- (lexer s >>= parseList)
  if rest == [] -- This is not good enough
    then Right e 
    else Left "Program not done"

parseList :: [Token] -> Either String (Expr, [Token])
parseList (Bracket LeftSquareBracket : Bracket RightSquareBracket : rest) = Right (Const (ConstList Empty), rest)
parseList (t : Operator ListCons : rest) = 
-- Really want to find a way to make this nicer
  case parseList rest of 
    Right (Const (ConstList Empty), rest') ->  
      case t of 
        Num n     -> Right (Const $ ConstList (Cons (Number n) Empty), rest')
        Boolean b -> Right (Const $ ConstList (Cons (Boole b) Empty), rest')
        _         -> Left $ "Parser error: Unable to construct list" ++ (show rest)
    Right (Const (ConstList l@(Cons (Number _) _)), rest')  -> 
      case t of 
        Num n     -> Right (Const $ ConstList (Cons (Number n) l), rest')
        Boolean _ -> Left $ "Parser error: Lists cannot have multiple types"
        _         -> Left $ "Parser error: Unable to construct list" ++ (show rest)
    Right (Const (ConstList l@(Cons (Boole _) _)), rest')  -> 
      case t of 
        Boolean b -> Right (Const $ ConstList (Cons (Boole b) l), rest')
        Num _     -> Left $ "Parser error: Lists cannot have multiple types"
        _         -> Left $ "Parser error: Unable to construct list" ++ (show rest)
    Right (e, rest')                    -> Left $ "Parser error: Unable to construct list" ++ (show rest)
    Left s                              -> Left s
parseList (Bracket LeftSquareBracket : t : rest) =
  case parseList' rest of 
    Left s                              -> Left s
    Right (Const (ConstList l), rest')  -> 
      case t of 
        Num n     -> Right (Const $ ConstList (Cons (Number n) l), rest')
        Boolean b -> Right (Const $ ConstList (Cons (Boole b) l), rest')
        _         -> Left $ "Parser error: Unable to construct list"
  where 
    parseList' :: [Token] -> Either String (Expr, [Token])
    parseList' (Bracket RightSquareBracket : rest) = Right $ (Const $ ConstList Empty, rest)
    parseList' (Operator Comma : Num n : rest) = do 
      (Const (ConstList l), rest') <- parseList' rest
      Right (Const (ConstList (Cons (Number n) l)), rest')
    parseList' t = Left $ "Parser error: Unable to construct list" ++ (show t)
parseList t@(Num n : Operator op : rest) = undefined
  -- case parseArithmic t of 
  --   Right  

-- This function will parse the basic boolean and arithmic languages 
parseBasic :: [Token] -> Either String (Expr, [Token])
parseBasic tokens = 
  case parseArithmic tokens of 
    Right e     -> return e
    Left s      -> 
      case parseBoolean tokens of 
        Right e     -> return e 
        Left s      -> Left $ "Parser error: Could not be parsed as a basic expression"

-- The B parser is used for parsing boolean expressions
-- parseBoolean and parseBooleanString are helper functions to access the sub-language
parseBoolean :: [Token] -> Either String (Expr, [Token])
parseBoolean tokens = parseBExpr tokens

parseBooleanString :: String -> Either String Expr
parseBooleanString s = lexer s >>= (\t -> fst `fmap` parseBoolean t)

parseBCExpr :: [Token] -> Either String (Expr, [Token])
parseBCExpr (Boolean True : rest) = return (Const (Boole True), rest)
parseBCExpr (Boolean False : rest) = return (Const (Boole False), rest)
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
parseArithmicString :: String -> Either String Expr
parseArithmicString s = lexer s >>= (\t -> fst `fmap` parseArithmic t) 

parseArithmic :: [Token] -> Either String (Expr, [Token])
parseArithmic tokens = parseSExpr tokens

-- The F, P, and S expression are used to handle the basic alrithmic operations
-- F expressions
parseFExpr :: [Token] -> Either String (Expr, [Token])
parseFExpr (Num (Integer i) : rest)   = return (Const (Number (Integer i)), rest)
parseFExpr (Num (Floating n) : rest) = return (Const (Number (Floating n)), rest)
parseFExpr (Identifier x : rest)  = return (Var x, rest)
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
            (Const (Number (Floating 0)):xs)   -> Left "Arithmic error: Divide by zero"
            (Const (Number (Integer 0)):xs)   -> Left "Arithmic error: Divide by zero"
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