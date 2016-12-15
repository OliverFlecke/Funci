module Lexer where 

import Data.Char

-- This is used for the names of the identifiers
type Name = String 

-- Tokens which the input can be transformed into
data Token = 
    Num Float  -- How do I get two different tokens for ints and floats?
    | Keyword Keywords 
    | Operator Operators 
    | Identifier Name 
    | Bracket Brackets
    | Semicolon 
    deriving (Show, Eq)

-- Keywords in the language
data Keywords = 
    If 
    | Then 
    | Else 
    | Where
    | Case 
    | Of 
    | Let 
    | In 
    deriving (Show, Eq)

-- Default operators in the languages
data Operators = 
    Plus 
    | Minus 
    | Times 
    | Divide 
    | Reminder
    | Assignment 
    deriving (Show, Eq)

-- Different kinds of brackets 
data Brackets = 
    LeftParen
    | RightParen
    | LeftBracket 
    | RightBracket
    | LeftSquareBracket
    | RightSquareBracket
    deriving (Show, Eq)

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
lexer ('+':cs) = addToken (Operator Plus) cs
lexer ('-':cs) = addToken (Operator Minus) cs
lexer ('*':cs) = addToken (Operator Times) cs
lexer ('/':cs) = addToken (Operator Divide) cs
lexer ('%':cs) = addToken (Operator Reminder) cs
lexer ('=':cs) = addToken (Operator Assignment) cs

-- Find digits in the string 
lexer (c : cs) | isDigit c = 
    let (digits, cs') = break (not . isDigitOrDot) $ c:cs
    in addToken (Num (read digits)) cs'
    where 
        isDigitOrDot :: Char -> Bool 
        isDigitOrDot c = isDigit c || c == '.'

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

