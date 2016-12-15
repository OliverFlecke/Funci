module Lexer where 

import Data.Char

-- This is used for the names of the identifiers
type Name = String 

-- Tokens which the input can be transformed into
data Token = 
    Num Int 
    | Keyword Keywords 
    | Operator Operators 
    | Identifier Name 
    | Bracket Brackets
    | Semicolon 
    | Unknown String String 
    deriving (Show, Eq)

-- Keywords in the language
data Keywords = 
    If 
    | Then 
    | Else 
    | While 
    | Return 
    | Int 
    deriving (Show, Eq)

-- Default operators in the languages
data Operators = 
    Plus 
    | Minus 
    | Times 
    | Divide 
    | Modulo
    | Assignment 
    deriving (Show, Eq)

data Brackets = 
    LeftParen
    | RightParen
    | LeftBracket 
    | RightBracket
    | LeftSquareBracket
    | RightSquareBracket
    deriving (Show, Eq)


-- Function to convert a string into a list of tokens
lexer :: String -> Either String [Token]
lexer "" = Right []

-- Handling whitespace
lexer (' ':xs) = lexer xs
lexer ('\n':xs) = lexer xs
lexer ('\t':xs) = lexer xs

-- Handling brackets
lexer ('(':xs) = addToken (Bracket LeftParen) xs
lexer (')':xs) = addToken (Bracket RightParen) xs
lexer ('[':xs) = addToken (Bracket LeftSquareBracket) xs
lexer (']':xs) = addToken (Bracket RightSquareBracket) xs
lexer ('{':xs) = addToken (Bracket LeftBracket) xs 
lexer ('}':xs) = addToken (Bracket RightBracket) xs 

lexer ('+':xs) = addToken (Operator Plus) xs
lexer ('-':xs) = addToken (Operator Minus) xs
lexer ('*':xs) = addToken (Operator Times) xs
lexer ('/':xs) = addToken (Operator Divide) xs
lexer ('%':xs) = addToken (Operator Modulo) xs

lexer s = Left $ "Unexpected character: \t" ++ s 

addToken :: Token -> String -> Either String [Token]
addToken t s = lexer s >>= (\ts -> Right $ t : ts)