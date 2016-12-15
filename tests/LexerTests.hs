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
    testLexer "%" [Operator Reminder]
    testLexer "=" [Operator Assignment]

    testLexer "10" [Num 10]
    testLexer "1.2" [Num 1.2]
    
    testLexer "if" [Keyword If]
    testLexer "then" [Keyword Then]
    testLexer "else" [Keyword Else]
    -- Note: This should actually fail later on
    testLexer "if then else" [Keyword If, Keyword Then, Keyword Else]
    testLexer "where" [Keyword Where]
    testLexer "case of" [Keyword Case, Keyword Of]
    testLexer "let in" [Keyword Let, Keyword In]
    testLexer "abc" [Identifier "abc"]
    testLexer "a = 10" [Identifier "a", Operator Assignment, Num 10]
    testLexer "$" []
    testLexer "-- This is a comment * + \n if" [Keyword If]

-- This is a temporary way to test lexer until I get online
-- No way good, need to be automated
testLexer :: String -> [Token] -> IO () 
testLexer s e = do 
    putStr $ "Testing: " ++ s 
    let tokens = lexer s 
    case tokens of 
        Left s' -> putStrLn $ "\n\t-- Lexing error --\n" ++ s' ++ "\n"
        Right tokens' -> if tokens' == e 
            then putStrLn $ " \t\t\tValid" 
            else putStrLn $ " \t-- Invalid - Should have been: \t" ++ (show e) 


-- test1 = "let x = 10 in x"
-- test2 = "10 + 20"