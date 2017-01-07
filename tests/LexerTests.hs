module Main where

import Lexer
import Syntax
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO () 
main = do
  testWhiteSpace
  testBrackets
  testArithmicOperators
  testCompareOperators
  testListOperators
  testBoolean
  testNumbers
  testKeywords
  testComments
  testArithmics
  testTypes
  testIdentifiers
  numbersWithUnits
  -- testLists

-- Test that whitespace is removed 
testWhiteSpace = hspec $ do 
  describe "Testing if whitespace is remvod properly" $ do 
    it "Nothing at all" $ lexer "" `shouldBe` Right []
    it "Space" $ lexer " " `shouldBe` Right []
    it "New line" $ lexer "\n" `shouldBe` Right []
    it "Tab" $ lexer "\t" `shouldBe` Right []

-- Test that brackets are lexer corretly 
testBrackets = hspec $ do 
  describe "Testing Brackets and paren" $ do 
    it "Parent" $ lexer "( )" `shouldBe` Right [Bracket LeftParen, Bracket RightParen]
    it "Brackets" $ lexer "{}" `shouldBe` Right [Bracket LeftBracket, Bracket RightBracket]
    it "Square brackets" $ lexer "[]" `shouldBe` Right [Bracket LeftSquareBracket, Bracket RightSquareBracket]

-- Test that the arithmic operators are represented correctly
testArithmicOperators = hspec $ do
  describe "Testing the different arithmic operators:" $ do
    it "Addition" $ lexer "+" `shouldBe` Right [Operator Add]
    it "Subtraction" $ lexer "-" `shouldBe` Right [Operator Sub]
    it "Multiplcation" $ lexer "*" `shouldBe` Right [Operator Mul]
    it "Divition" $ lexer "/" `shouldBe` Right [Operator Div]
    it "Reminder" $ lexer "%" `shouldBe` Right [Operator Mod]
    it "Power" $ lexer "^" `shouldBe` Right [Operator Pow]
    it "Assignment" $ lexer "=" `shouldBe` Right [Operator Assignment]
    it "Type Assignment" $ lexer "::" `shouldBe` Right [Operator TypeAssignment]
    it "Type arrow" $ lexer "->" `shouldBe` Right [Operator TypeArrow]

-- Test comparation operators
testCompareOperators = hspec $ do 
  describe "Testing the comparator operators:" $ do 
    it "Equality " $ lexer "==" `shouldBe` Right [Operator Eq]
    it "Not equal " $ lexer "!=" `shouldBe` Right [Operator Ne]
    it "Less than " $ lexer "<" `shouldBe` Right [Operator Lt] 
    it "Greater than " $ lexer ">" `shouldBe` Right [Operator Gt]
    it "Less than or equal " $ lexer "<=" `shouldBe` Right [Operator Le]
    it "Greater than or equal " $ lexer ">=" `shouldBe` Right [Operator Ge]

-- Testing list operators
testListOperators = hspec $ do 
  describe "Testing list operators:" $ do 
    it "Head" $ lexer "head" `shouldBe` Right [Operator Head]
    it "Tail" $ lexer "tail" `shouldBe` Right [Operator Tail]
    it "ListCons" $ lexer ":" `shouldBe` Right [Operator ListCons]
    it "Comma" $ lexer "," `shouldBe` Right [Operator Comma]
    it "isEmpty" $ lexer "isEmpty" `shouldBe` Right [Operator IsEmpty] 

-- Testing Boolean operators
testBoolean = hspec $ do
  describe "Testing boolean constants" $ do 
    it "True" $ lexer "True" `shouldBe` Right [Booly True]
    it "False" $ lexer "False" `shouldBe` Right [Booly False]
  describe "Testing the booloan operators:" $ do 
    it "And" $ lexer "&&" `shouldBe` Right [Operator And]
    it "Or" $ lexer "||" `shouldBe` Right [Operator Or]
    it "Not" $ lexer "!" `shouldBe` Right [Operator Not]
  describe "Testing boolean expression lexing" $ do 
    it "True and True" $ lexer "True && True" `shouldBe` Right [Booly True, Operator And, Booly True]
    it "True or False" $ lexer "True || False" `shouldBe` Right [Booly True, Operator Or, Booly False]
    it "Not True" $ lexer "!True" `shouldBe` Right [Operator Not, Booly True]
    it "Not False" $ lexer "!False" `shouldBe` Right [Operator Not, Booly False]

-- Test that numbers are being converted correctly 
testNumbers = hspec $ do 
  describe "Testing number conversions" $ do 
    it "Integers" $ do
      lexer "10" `shouldBe` Right [Num 10]
      lexer "123456" `shouldBe` Right [Num 123456]
    it "Floating points" $ do
      lexer "12.34" `shouldBe` Right [Num 12.34]
      lexer "12.84" `shouldBe` Right [Num 12.84]
    -- it "Floating points with mulitipul '.'. Should throw an error (Left)" $ do
    --   -- lexer "12.34.5" `shouldBe` Left "Could not parse: number contains too many '.'" 
    --   lexer "12.34.5" `shouldThrow` anyException

-- Test that the keywords are represented correctly
testKeywords = hspec $ do
  describe "Conveting simple keywords" $ do 
    it "Convert each keyword into its tokenized version" $ do 
      lexer "if" `shouldBe` Right [Keyword If]
      lexer "then" `shouldBe` Right [Keyword Then]
      lexer "else" `shouldBe` Right [Keyword Else]
      -- This should maybe not fail later on, if the lexer should catch these errors
      lexer "if then else" `shouldBe` Right [Keyword If, Keyword Then, Keyword Else]   
      lexer "where" `shouldBe` Right [Keyword Where]
      lexer "case of" `shouldBe` Right [Keyword Case, Keyword Of]
      lexer "let in" `shouldBe` Right [Keyword Let, Keyword In]
      lexer "abc" `shouldBe` Right [Identifier "abc"]
      lexer "a = 10" `shouldBe` Right [Identifier "a", Operator Assignment, Num 10]
      lexer "$" `shouldBe` Left "Unexpected character: \t$\n                      \t^"
    it "Valid expressions" $ do 
      lexer "let x = 1 in x" `shouldBe` Right [Keyword Let, Identifier "x", Operator Assignment, Num 1, Keyword In, Identifier "x"]

-- Test that comments are removed
testComments = hspec $ do 
  describe "Testing that comments are removed" $ do
    it "Should only return the code, not the comment" $ do
      lexer "-- This is a comment * + \n" `shouldBe` Right []
      lexer "-- This is a comment * + \n3" `shouldBe` Right [Num 3]

-- Test that arithmics are lexed correctly
testArithmics = hspec $ do 
  describe "Testing the lexing of arithmic strings" $ do 
    it "Testing each operator with numbers" $ do 
      lexer "2 + 3" `shouldBe` Right [Num 2, Operator Add, Num 3]
      lexer "1 - 2" `shouldBe` Right [Num 1, Operator Sub, Num 2]
      lexer "1 * 2" `shouldBe` Right [Num 1, Operator Mul, Num 2]
      lexer "1 / 2" `shouldBe` Right [Num 1, Operator Div, Num 2]
      lexer "2 ^ 2" `shouldBe` Right [Num 2, Operator Pow, Num 2]
    it "Test cases with interleaving operators" $ do
      lexer "1 + 2 * 3" `shouldBe` Right [Num 1, Operator Add, Num 2, Operator Mul, Num 3]
      lexer "1 * 2 + 3" `shouldBe` Right [Num 1, Operator Mul, Num 2, Operator Add, Num 3]
      lexer "1 / 2 - 3" `shouldBe` Right [Num 1, Operator Div, Num 2, Operator Sub, Num 3]
      lexer "1 - 2 / 3" `shouldBe` Right [Num 1, Operator Sub, Num 2, Operator Div, Num 3]
    it "Test cases with floating point numbers" $ do 
      lexer "1.2 + 3" `shouldBe` Right [Num 1.2, Operator Add, Num 3]
      lexer "1 + 2.3" `shouldBe` Right [Num 1, Operator Add, Num 2.3]
      lexer "1.2 + 3.4" `shouldBe` Right [Num 1.2, Operator Add, Num 3.4]
      lexer "1.2 * 3.4" `shouldBe` Right [Num 1.2, Operator Mul, Num 3.4]

testTypes = hspec $ do 
  describe "Testing types" $ do 
    it "Int" $ lexer "Int" `shouldBe` Right [BType Int]
    it "Float" $ lexer "Float" `shouldBe` Right [BType Float]
    it "Bool" $ lexer "Bool" `shouldBe` Right [BType Bool]
    it "Unit" $ lexer "()" `shouldBe` Right [BType UnitType]
    it "Int -> Int" $ lexer "Int -> Int" `shouldBe` Right [BType Int, Operator TypeArrow, BType Int]

testIdentifiers = hspec $ do 
  describe "Finding identifiers -" $ do 
    it "a" $ lexer "a" `shouldBe` Right [Identifier "a"]
    it "abc" $ lexer "abc" `shouldBe` Right [Identifier "abc"]
    it "a1a" $ lexer "a1a" `shouldBe` Right [Identifier "a1a"]
-- testLists = hspec $ do 
--   describe "Testing basic list construction:" $ do 
--     -- it "The empty list" $ lexer "[]" `shouldBe` Right []
--     it


-- Testing the lexing of numbers with units
numbersWithUnits = hspec $ do 
  describe "Testing lexing of numbers with units" $ do 
    it "1 #" $ lexer "1 <<m>>" `shouldBe` Right [Num 1, Units "m"]
    it "1 # m s A #" $ lexer "1 <<m s A>>" `shouldBe` Right [Num 1, Units "m s A"] 