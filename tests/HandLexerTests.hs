module Main where

import HandLexer
import HandSyntax
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO () 
main = hspec $ do 
    testWhiteSpace
    testBrackets
    testArithmicOperators
    testCompareOperators
    testBooleanOperators
    testNumbers
    testKeywords
    testComments
    testArithmics

-- Test that whitespace is removed 
testWhiteSpace = 
    describe "Testing if whitespace is remvod properly" $ do 
        it "Nothing at all" $ lexer "" `shouldBe` Right []
        it "Space" $ lexer " " `shouldBe` Right []
        it "New line" $ lexer "\n" `shouldBe` Right []
        it "Tab" $ lexer "\t" `shouldBe` Right []

-- Test that brackets are lexer corretly 
testBrackets = 
    describe "Testing Brackets and paren" $ do 
        it "Parent" $ lexer "()" `shouldBe` Right [Bracket LeftParen, Bracket RightParen]
        it "Brackets" $ lexer "{}" `shouldBe` Right [Bracket LeftBracket, Bracket RightBracket]
        it "Square brackets" $ lexer "[]" `shouldBe` Right [Bracket LeftSquareBracket, Bracket RightSquareBracket]

-- Test that the arithmic operators are represented correctly
testArithmicOperators =
    describe "Testing the different arithmic operators:" $ do
        it "Addition" $ lexer "+" `shouldBe` Right [Operator Add]
        it "Subtraction" $ lexer "-" `shouldBe` Right [Operator Sub]
        it "Multiplcation" $ lexer "*" `shouldBe` Right [Operator Mul]
        it "Divition" $ lexer "/" `shouldBe` Right [Operator Div]
        it "Reminder" $ lexer "%" `shouldBe` Right [Operator Rem]
        it "Assignment" $ lexer "=" `shouldBe` Right [Operator Assignment]

-- Test comparation operators
testCompareOperators = 
    describe "Testing the comparator operators:" $ do 
        it "Equality " $ lexer "==" `shouldBe` Right [Operator Eq]
        it "Not equal " $ lexer "!=" `shouldBe` Right [Operator Ne]
        it "Less than " $ lexer "<" `shouldBe` Right [Operator Lt] 
        it "Greater than " $ lexer ">" `shouldBe` Right [Operator Gt]
        it "Less than or equal " $ lexer "<=" `shouldBe` Right [Operator Le]
        it "Greater than or equal " $ lexer ">=" `shouldBe` Right [Operator Ge]

-- Testing Boolean operators
testBooleanOperators = 
    describe "Testing the booloan operators:" $ do 
        it "And" $ lexer "&&" `shouldBe` Right [Operator And]
        it "Or" $ lexer "||" `shouldBe` Right [Operator Or]
        it "Not" $ lexer "!" `shouldBe` Right [Operator Not]

-- Test that numbers are being converted correctly 
testNumbers = 
    describe "Testing number conversions" $ do 
        it "Integers" $ do
            lexer "10" `shouldBe` Right [Num 10]
            lexer "123456" `shouldBe` Right [Num 123456]
        it "Floating points" $ do
            lexer "12.34" `shouldBe` Right [Num 12.34]
            lexer "12.84" `shouldBe` Right [Num 12.84]
        it "Floating points with mulitipul '.'. Should throw an error (Left)" $ do
            lexer "12.34.5" `shouldBe` Left "Could not parse: number contains too many '.'" 

-- Test that the keywords are represented correctly
testKeywords =
    describe "Conveting simple keywords" $ do 
        it "should convert each keyword into its tokenized version" $ do 
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
   
-- Test that comments are removed
testComments = 
    describe "Testing that comments are removed" $ do
        it "Should only return the code, not the comment" $ do
            lexer "-- This is a comment * + \n" `shouldBe` Right []
            lexer "-- This is a comment * + \n3" `shouldBe` Right [Num 3]

-- Test that arithmics are lexed correctly
testArithmics = 
    describe "Testing the lexing of arithmic strings" $ do 
        it "Testing each operator with numbers" $ do 
            lexer "2 + 3" `shouldBe` Right [Num 2, Operator Add, Num 3]
            lexer "1 - 2" `shouldBe` Right [Num 1, Operator Sub, Num 2]
            lexer "1 * 2" `shouldBe` Right [Num 1, Operator Mul, Num 2]
            lexer "1 / 2" `shouldBe` Right [Num 1, Operator Div, Num 2]
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