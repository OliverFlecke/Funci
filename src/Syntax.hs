module Syntax where

type Program = [Bind] 
type Id = String 

data Bind = Bind Id (Maybe QType) [Id] Expr
  deriving (Read, Show, Eq, Ord)

data NumType = Integer Int 
  | Floating Float 
  deriving (Read, Show, Eq, Ord)

data Type = 
  Number NumType
  | Boole Bool
  | ConstList (List Type)
  deriving (Read, Show, Eq, Ord)

data QType = Forall Id QType
            | Ty FType 
            deriving (Read, Show, Eq, Ord)

data FType =  Arrow FType FType
            | Prod FType FType
            | Sum FType FType
            | Base BaseType
            | TypeVar Id 
            deriving (Read, Show, Eq, Ord)

data BaseType = Unit
              | Int 
              | Float
              | Bool 
              deriving (Read, Show, Eq, Ord)


-- Tokens which the input can be transformed into
data Token = 
  Num NumType 
  | Boolean Bool
  | Keyword Keywords 
  | Operator Operator 
  | Identifier Id 
  | Bracket Brackets
  | Semicolon 
  deriving (Read, Show, Eq, Ord)

data List a = Empty | Cons a (List a) deriving (Read, Show, Eq, Ord)

-- And this is what the parser should output 
-- (eventually turned into a valid program)
data Expr = 
  Const Type
  | Var Id 
  | Prim Operator
  | App Expr Expr 
  | LetIn [(Id,Expr)] Expr 
  | IfThenElse Expr Expr Expr 
  deriving (Read, Show, Eq, Ord)

data Keywords = 
  If | Then | Else 
  | Case | Of 
  | Let | In 
  | Where
  deriving (Read, Show, Eq, Ord)

data Operator = 
  Add | Sub | Mul | Div | Mod
  | Gt | Ge | Lt | Le | Eq | Ne
  | And | Or | Not  
  | Head | Tail | ListCons
  | Comma 
  | Assignment | TypeAssignment 
  deriving (Read, Show, Eq, Ord)

data Brackets = 
  LeftParen | RightParen
  | LeftBracket | RightBracket
  | LeftSquareBracket | RightSquareBracket
  deriving (Read, Show, Eq, Ord)