module HandLexer where 

import Data.Char
import HandSyntax

addToken :: Token -> String -> Either String [Token]
addToken t s = lexer s >>= (\ts -> Right $ t : ts)

-- Function to convert a string into a list of tokens
lexer :: String -> Either String [Token]
lexer "" = Right []

-- Handling whitespace
lexer (' ':cs) = lexer cs
lexer ('\n':cs) = lexer cs
lexer ('\t':cs) = lexer cs

-- Remove comments
lexer ('-':'-':cs) = 
  let (_, cs') = break (\c -> c == '\n') cs -- Rewrite this inline function please...
  in lexer cs'

-- Not sure if I want to have semicolons in the language
lexer (';':cs) = addToken Semicolon cs

-- Handling brackets
lexer ('(':cs) = addToken (Bracket LeftParen) cs
lexer (')':cs) = addToken (Bracket RightParen) cs
lexer ('[':cs) = addToken (Bracket LeftSquareBracket) cs
lexer (']':cs) = addToken (Bracket RightSquareBracket) cs
lexer ('{':cs) = addToken (Bracket LeftBracket) cs 
lexer ('}':cs) = addToken (Bracket RightBracket) cs 

-- Handing operators
lexer ('=':'=':cs) = addToken (Operator Eq) cs
lexer ('!':'=':cs) = addToken (Operator Ne) cs
lexer ('<':'=':cs) = addToken (Operator Le) cs
lexer ('>':'=':cs) = addToken (Operator Ge) cs
lexer ('<':cs) = addToken (Operator Lt) cs
lexer ('>':cs) = addToken (Operator Gt) cs

lexer ('&':'&':cs) = addToken (Operator And) cs
lexer ('|':'|':cs) = addToken (Operator Or) cs
lexer ('!':cs) = addToken (Operator Not) cs

lexer ('+':cs) = addToken (Operator Add) cs
lexer ('-':cs) = addToken (Operator Sub) cs
lexer ('*':cs) = addToken (Operator Mul) cs
lexer ('/':cs) = addToken (Operator Div) cs
lexer ('%':cs) = addToken (Operator Rem) cs
lexer ('=':cs) = addToken (Operator Assignment) cs

lexer ('h':'e':'a':'d':cs) = addToken (Operator Head) cs
lexer ('t':'a':'i':'l':cs) = addToken (Operator Tail) cs
lexer (':':cs) = addToken (Operator ListCons) cs

lexer (',':cs) = addToken (Operator Comma) cs

-- Find boolean values
lexer ('T':'r':'u':'e' : cs) = addToken (Boolean True) cs
lexer ('F':'a':'l':'s':'e' : cs) = addToken (Boolean False) cs

-- Find digits in the string 
lexer (c : cs) | isDigit c = 
  let (digits, cs') = break (not . isDigitOrDot) $ c:cs
  in case countDots digits of 
    0 -> addToken (Num (Int (read digits))) cs'
    1 -> addToken (Num (Float (read digits))) cs'
    _ -> Left "Could not parse: number contains too many '.'"
  where 
    isDigitOrDot :: Char -> Bool 
    isDigitOrDot c = isDigit c || c == '.'
    countDots :: [Char] -> Int
    countDots = length . filter (\d -> d == '.')

-- Find names in the string
lexer (c : cs) | isLetter c = 
  let (id, cs') = break (not . isLetter) $ c:cs
  in addToken (toIdOrKeyword id) cs'
  where
    toIdOrKeyword :: String -> Token
    toIdOrKeyword s 
      | s == "if"     = Keyword If 
      | s == "then"   = Keyword Then 
      | s == "else"   = Keyword Else 
      | s == "case"   = Keyword Case 
      | s == "of"     = Keyword Of 
      | s == "let"    = Keyword Let 
      | s == "in"     = Keyword In 
      | s == "where"  = Keyword Where 
      | otherwise     = Identifier s

-- Error handling 
-- Is it possible to get a line and/or column number 
lexer s = Left $ "Unexpected character: \t" ++ s 
      ++ "\n                      \t^"

