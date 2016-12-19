module Main where

import HandLexer
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
            lexer "if then else" `shouldBe` Right [Keyword If, Keyword Then, Keyword Else]   
            lexer "where" `shouldBe` Right [Keyword Where]
--     testLexer "if" [Keyword If]
--     testLexer "then" [Keyword Then]
--     testLexer "else" [Keyword Else]
--     -- Note: This should actually fail later on
--     testLexer "if then else" [Keyword If, Keyword Then, Keyword Else]
--     testLexer "where" [Keyword Where]
--     testLexer "case of" [Keyword Case, Keyword Of]
--     testLexer "let in" [Keyword Let, Keyword In]
--     testLexer "abc" [Identifier "abc"]
--     testLexer "a = 10" [Identifier "a", Operator Assignment, Num 10]
--     testLexer "$" []
--     testLexer "-- This is a comment * + \n if" [Keyword If]

-- -- This is a temporary way to test lexer until I get online
-- -- No way good, need to be automated
-- testLexer :: String -> [Token] -> IO () 
-- testLexer s e = do 
--     putStr $ "Testing: " ++ s 
--     let tokens = lexer s 
--     case tokens of 
--         Left s' -> putStrLn $ "\n\t-- Lexing error --\n" ++ s' ++ "\n"
--         Right tokens' -> if tokens' == e 
--             then putStrLn $ " \t\t\tValid" 
--             else putStrLn $ " \t-- Invalid - Should have been: \t" ++ (show e) 

