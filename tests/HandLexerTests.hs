module Main where

import HandLexer
import Syntax
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO () 
main = hspec $ do 
    describe "Empty string" $ do 
        it "should return the empty list" $ do
            lexer "" `shouldBe` Right []
    describe "A pairs of brackets" $ do 
        it "should return the tokenized version" $ do
            lexer "()" `shouldBe` Right [Bracket LeftParen, Bracket RightParen]
            lexer "[]" `shouldBe` Right [Bracket LeftSquareBracket, Bracket RightSquareBracket]
            lexer "{}" `shouldBe` Right [Bracket LeftBracket, Bracket RightBracket]
    describe "Testing the different operators" $ do
        it "should return the tokenized operator" $ do 
            lexer "+" `shouldBe` Right [Operator Plus]
            lexer "-" `shouldBe` Right [Operator Minus]
            lexer "*" `shouldBe` Right [Operator Times]
            lexer "/" `shouldBe` Right [Operator Divide]
            lexer "%" `shouldBe` Right [Operator Reminder]
            lexer "=" `shouldBe` Right [Operator Assignment]
    describe "Nummeric tests" $ do 
        it "should convert the strings into numbers" $ do 
            lexer "10" `shouldBe` Right [Num 10]
            lexer "123456" `shouldBe` Right [Num 123456]
            lexer "12.34" `shouldBe` Right [Num 12.34]
        it "should through an error (Left)" $ do
            lexer "12.34.5" `shouldBe` Left "Could not parse: number contains too many '.'" 
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
    describe "Testing that comments are removed" $ do
        it "Should only return the code, not the comment" $ do
            lexer "-- This is a comment * + \n if" `shouldBe` Right [Keyword If]
