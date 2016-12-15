module Main where

import Lexer 

main :: IO ()
-- main = putStrLn "This is still just a dummy function. Need to setup a good way to test everything"
main = do 
    print $ lexer ""
    print $ lexer "\n"
    print $ lexer "\t"
    print $ lexer "()"
    print $ lexer "{}"
    print $ lexer "[]"
    print $ lexer "if 10 > 8 then True else False" 
    putStrLn "done"
