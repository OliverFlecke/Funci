module Syntax where

import Data.List
import qualified Environment as E

type (VEnv a) = E.Env (Value a)
type Program a = [Bind a]
type Id = String

data Bind a = Bind Id (Maybe QType) [Id] (Expr a)
  deriving (Read, Show, Eq, Ord)

-- Tokens which the input can be transformed into
data Token a =
  Num a
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

-- This is the values which the program should be able to return
data Value a = Number a Unit
           | Boolean Bool
           | Listy (List (Value a))
           | Fun (VEnv a) [Id] (Expr a)
           | P Operator [Value a]
           | C Id [Value a]
           deriving (Read, Show, Eq, Ord)

-- And this is what the parser should output
-- (eventually turned into a valid program)
data Expr a =
  End
  | Const (Value a)
  | Var Id
  | Prim Operator
  | App (Expr a) (Expr a)
  | LetIn [Bind a] (Expr a)
  | IfThenElse (Expr a) (Expr a) (Expr a)
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
data UnitPrefix =
    Yocto   -- 10^-24 y
  | Zepto   -- 10^-21 z
  | Atto    -- 10^-18 a
  | Femto   -- 10^-15 f
  | Pico    -- 10^-12 p
  | Nano    -- 10^-9  n
  | Micro   -- 10^-6  mu
  | Milli   -- 10^-3  m
  | Centi   -- 10^-2  c
  | Deci    -- 10^-1  d
  | None
  | Deca    -- 10^1   da
  | Hecto   -- 10^2   h
  | Kilo    -- 10^3   k
  | Mega    -- 10^6   M
  | Giga    -- 10^9   G
  | Tera    -- 10^12  T
  | Peta    -- 10^15  P
  | Exa     -- 10^18  E
  | Zetta   -- 10^21  Z
  | Yotta   -- 10^24  Y
  deriving (Enum, Read, Show, Eq, Ord)

  -- Printing values in a nice
printValue :: (Show a) => Value a -> IO ()
printValue v = putStrLn $ formatValue v

formatValue :: (Show a) => Value a -> String
formatValue (Number n u)  = (show n) ++ (formatUnit u)
formatValue (Boolean b)   = (show b)
formatValue (Listy l)     = prettyList l
formatValue _             = error "Value is not supposed to be outputted"

prettyList :: (Show a) => List (Value a) -> String
prettyList l = "[" ++ (intercalate ", " $ prettyList' l) ++ "]"
  where
    prettyList' Empty       = []
    prettyList' (Cons v l)  = (formatValue v) : (prettyList' l)

formatUnit :: Unit -> String
formatUnit (Unit [])  = ""
formatUnit u          = " <<" ++ (intercalate " * " $ formatUnit' u) ++ ">>"
  where
    formatUnit' (Unit [])     = []
    formatUnit' (Unit (u:us)) = (prettyUnit u) : formatUnit' (Unit us)

prettyUnit :: (BaseUnit, UnitPrefix, Exponent) -> String
prettyUnit (u, p, e) = (prefixToString p) ++ (baseUnitToString u) ++ (expoString e)
  where expoString e = if e == 1 then "" else ("^" ++ (show e))

prefixToString :: UnitPrefix -> String
prefixToString None = ""
prefixToString Yocto  = "y"
prefixToString Zepto  = "z"
prefixToString Atto   = "a"
prefixToString Femto  = "f"
prefixToString Pico   = "p"
prefixToString Nano   = "n"
prefixToString Micro  = "mu"
prefixToString Milli  = "m"
prefixToString Centi  = "c"
prefixToString Deci   = "d"
prefixToString Deca   = "da"
prefixToString Hecto  = "h"
prefixToString Kilo   = "k"
prefixToString Mega   = "M"
prefixToString Giga   = "G"
prefixToString Tera   = "T"
prefixToString Peta   = "P"
prefixToString Exa    = "E"
prefixToString Zetta  = "Z"
prefixToString Yotta  = "Y"

baseUnitToString :: BaseUnit -> String
baseUnitToString Metre    = "m"
baseUnitToString Second   = "s"
baseUnitToString Gram     = "g"
baseUnitToString Ampere   = "A"
baseUnitToString Kelvin   = "K"
baseUnitToString Mole     = "mol"
baseUnitToString Candela  = "cd"
baseUnitToString (CustomUnit s) = s