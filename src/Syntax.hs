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
  | Units String
  | Identifier Id
  | Bracket Brackets
  | Semicolon
  | UnitApply
  deriving (Read, Show, Eq, Ord)

data List a = Empty | Cons a (List a) deriving (Read, Show, Eq, Ord)

type VEnv = E.Env Value
-- This is the values which the program should be able to return
data Value = Number NumType Unit
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
  Add | Sub | Mul | Div | Mod | Pow
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
data Unit = Unit [(BaseUnit, UnitPrefix, Exponent)]
  deriving (Read, Show, Eq, Ord)

type Exponent = Int

data BaseUnit =
  Metre       -- m
  | Second    -- s
  | Gram      -- g
  | Ampere    -- A
  | Kelvin    -- K
  | Mole      -- mol
  | Candela   -- cd
  | CustomUnit String
  deriving (Read, Show, Eq, Ord)

-- Stardard metric prefixes
data UnitPrefix = None
  | Yotta   -- 10^24  Y
  | Zetta   -- 10^21  Z
  | Exa     -- 10^18  E
  | Peta    -- 10^15  P
  | Tera    -- 10^12  T
  | Giga    -- 10^9   G
  | Mega    -- 10^6   M
  | Kilo    -- 10^3   k
  | Hecto   -- 10^2   h
  | Deca    -- 10^1   da
  | Deci    -- 10^-1  d
  | Centi   -- 10^-2  c
  | Milli   -- 10^-3  m
  | Micro   -- 10^-6  mu
  | Nano    -- 10^-9  n
  | Pico    -- 10^-12 p
  | Femto   -- 10^-15 f
  | Atto    -- 10^-18 a
  | Zepto   -- 10^-21 z
  | Yocto   -- 10^-24 y
  deriving (Enum, Read, Show, Eq, Ord)