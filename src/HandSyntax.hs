module HandSyntax where

type Program = Expr
-- data Bind = Bind Id Type [Id] Exp
--     deriving (Read, Show, Eq)

type Id = String 
-- data Value = 
--     Int 
--     | Float 
--     | Bool
--     deriving (Show, Eq)

-- Tokens which the input can be transformed into
data Token = 
    Num Float  -- How do I get two different tokens for ints and floats?
    | Boolean Bool
    | Keyword Keywords 
    | Operator Operator 
    | Identifier Id 
    | Bracket Brackets
    | Semicolon 
    deriving (Show, Eq)

data List = Empty | Cons Float List deriving (Show, Eq)

-- And this is what the parser should output 
-- (eventually turned into a valid program)
data Expr = 
    Const Float
    | ConstBool Bool
    | ConstList List    
    | Var Id 
    | Prim Operator
    | App Expr Expr 
    | LetIn Id Expr Expr 
    | IfThenElse Expr Expr Expr 
    deriving (Show, Eq)

data Keywords = 
    If | Then | Else 
    | Case | Of 
    | Let | In 
    | Where
    deriving (Show, Eq)

data Operator = 
    Add | Sub | Mul | Div | Rem
    | Gt | Ge | Lt | Le | Eq | Ne
    | And | Or | Not  
    | Head | Tail | ListCons
    | Comma
    | Assignment 
    deriving (Show, Eq)

data Brackets = 
    LeftParen | RightParen
    | LeftBracket | RightBracket
    | LeftSquareBracket | RightSquareBracket
    deriving (Show, Eq)