module Syntax where

import qualified Environment as E

type Program = [Bind] 
type Id = String 

data Bind = Bind Id (Maybe QType) [Id] Expr
  deriving (Read, Show, Eq, Ord)

data NumType = I Int 
  | F Float 
  deriving (Read, Show, Eq, Ord)

-- Tokens which the input can be transformed into
data Token = 
  Num NumType 
  | Booly Bool
  | Keyword Keywords 
  | Operator Operator 
  | Identifier Id 
  | Bracket Brackets
  | Semicolon 
  deriving (Read, Show, Eq, Ord)

data List a = Empty | Cons a (List a) deriving (Read, Show, Eq, Ord)

type VEnv = E.Env Value 
-- This is the values which the program should be able to return
data Value = Number NumType
           | Boolean Bool
           | Listy (List Value)
           | Fun VEnv [Id] Expr
           | P Operator [Value]
           | C Id [Value]
           deriving (Read, Show, Eq, Ord)

-- And this is what the parser should output 
-- (eventually turned into a valid program)
data Expr = 
  End 
  | Const Value
  | Var Id 
  | Prim Operator
  | App Expr Expr 
  | LetIn [Bind] Expr 
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

-- For the type checker
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