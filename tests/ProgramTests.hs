module Main where

import System.Environment
import System.Directory
import Text.Regex.Posix
import Evaluator
import Syntax
import Test.Hspec

main :: IO ()
main = findPrograms ["tests\\programs\\"]

findPrograms :: [String] -> IO ()
findPrograms [] = putStrLn "Done"
findPrograms ("." : ps)  = findPrograms ps
findPrograms (".." : ps) = findPrograms ps
findPrograms (p:ps)      = do
    dir <- getDirectoryContents p
    let dir' = filter removeDots dir
    let files = map (p ++) $ filter findFn dir'
    let outputs = map (p ++) $ filter findOutputs dir'
    runPrograms p (files, outputs)
    let dirs = ps ++ (map (++ "\\") (map (p ++) (filter findFiles dir')))
    -- putStrLn $ "Files: " ++ (show files) ++ "\tDirs: " ++ (show dirs)
    findPrograms dirs
  where 
    removeDots :: String -> Bool
    removeDots "."  = False 
    removeDots ".." = False
    removeDots _    = True 
    findFiles :: String -> Bool
    findFiles s = not $ findFn s || findOutputs s
    findFn :: String -> Bool
    findFn s = s =~ ".fn"
    findOutputs :: String -> Bool 
    findOutputs s = s =~ ".out"

runPrograms :: String -> ([String], [String]) -> IO ()
runPrograms path ([], [])     = putStrLn $ "Done with " ++ path
runPrograms path (p:ps, o:os) = do
  runProgram (p, o)
  runPrograms path (ps, os)
runPrograms _ _ = error "Missing test file"

runProgram :: (String, String) -> IO ()
runProgram (file, output) = do 
  program <- readFile file
  let result = evaluateString program
  expected <- readFile output
  let expected' = readValue expected
  hspec $ describe "Testing progam" $ it ("Program: " ++ program ++ "\n  Output:  " ++ (show expected')) $ result `shouldBe` expected' 
  where 
    readValue :: String -> Value 
    readValue s = read s