module Syntax where

import qualified Environment as E

type Program = [Bind] 
type Id = String 

data Bind = Bind Id (Maybe QType) [Id] Expr
  deriving (Read, Show, Eq, Ord)

data NumType = I Int | F Float 
  deriving (Read, Show, Eq, Ord)
  
-- Tokens which the input can be transformed into
data Token = 
  Num NumType 
  | Booly Bool
  | BType BaseType
  | Keyword Keywords 
  | Operator Operator 
  | Identifier Id 
  | Bracket Brackets
  | Semicolon 
  | UnitApply
  deriving (Read, Show, Eq, Ord)

data List a = Empty | Cons a (List a) deriving (Read, Show, Eq, Ord)

type VEnv = E.Env Value 
-- This is the values which the program should be able to return
data Value = Number NumType (Maybe Unit)
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
  | Head | Tail | ListCons | IsEmpty
  | Comma 
  | Assignment | TypeAssignment | TypeArrow 
  deriving (Read, Show, Eq, Ord)

data Brackets = 
  LeftParen | RightParen
  | LeftBracket | RightBracket
  | LeftSquareBracket | RightSquareBracket
  deriving (Read, Show, Eq, Ord)

-- For the type checker
data QType = Forall Id QType
            | Ty Type 
            deriving (Read, Show, Eq, Ord)

data Type = Arrow Type Type
          | Prod Type Type
          | Sum Type Type
          | Base BaseType
          | TypeVar Id 
          deriving (Read, Show, Eq, Ord)

data BaseType = UnitType
              | Int 
              | Float
              | Bool 
              deriving (Read, Show, Eq, Ord)

-- Units for number
data Unit = Unit UnitPrefix BaseUnit
  deriving (Read, Show, Eq, Ord)
  
data BaseUnit = 
  Meter 
  | Second
  | Grams
  | Ampere
  | Kelvin
  | Mole
  | Candela
  deriving (Read, Show, Eq, Ord)

-- Stardard metric prefixes
data UnitPrefix = 
  Exa       -- 10^18
  | Peta    -- 10^15
  | Tera    -- 10^12
  | Giga    -- 10^9
  | Mega    -- 10^6
  | Kilo    -- 10^3
  | Hecto   -- 10^2
  | Deca    -- 10^1
  | None
  | Deci    -- 10^-1
  | Centi   -- 10^-2
  | Milli   -- 10^-3
  | Micro   -- 10^-6
  | Nano    -- 10^-9
  | Pico    -- 10^-12
  | Femto   -- 10^-15
  | Atto    -- 10^-18
  deriving (Read, Show, Eq, Ord)