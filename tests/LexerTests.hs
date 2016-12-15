module Main where

import Lexer
-- import QuickCheck

main :: IO ()
-- main = print "We got the tests file running!"
main = do 
    testLexer "" []
    testLexer "()" [Bracket LeftParen, Bracket RightParen]
    testLexer "[]" [Bracket LeftSquareBracket, Bracket RightSquareBracket]
    testLexer "{}" [Bracket LeftBracket, Bracket RightBracket]
    testLexer "+" [Operator Plus]
    testLexer "-" [Operator Minus]
    testLexer "/" [Operator Divide]
    testLexer "*" [Operator Times]
    testLexer "%" [Operator Modulo]

    testLexer "if" [Keyword If]
    testLexer "then" [Keyword Then]
    testLexer "else" [Keyword Else]
    -- Note: This should actually fail later on
    testLexer "if then else" [Keyword If, Keyword Then, Keyword Else]

-- This is a temporary way to test lexer until I get online
-- No way good, need to be automated
testLexer :: String -> [Token] -> IO () 
testLexer s e = do 
    print $ "Testing: " ++ s 
    let tokens = lexer s 
    case tokens of 
        Left s' -> putStrLn $ "-- Something went wrong\n\t" ++ s' ++ "\n"
        Right tokens' -> putStrLn $ show (tokens' == e) ++ "\n" 


