module Main where

import System.Environment
import System.Directory
import Text.Regex.Posix
import Evaluator
import Syntax
import Test.Hspec
import Test.HUnit

main :: IO ()
main = findPrograms ["tests\\programs\\"]

findPrograms :: [String] -> IO ()
findPrograms [] = putStrLn "Testing is done"
findPrograms ("." : ps)  = findPrograms ps
findPrograms (".." : ps) = findPrograms ps
findPrograms (p:ps)      = do
    dir <- getDirectoryContents p
    let dir' = filter removeDots dir
    let files = map (p ++) $ filter findFn dir'
    let outputs = map (p ++) $ filter findOutputs dir'
    runPrograms p (files, outputs)
    let dirs = ps ++ (map (++ "\\") (map (p ++) (filter findFiles dir')))
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
-- runPrograms path files = do 
--   tests <- generateTestCases path files
--   count <- runTestTT tests
--   putStrLn (showCounts count)
runPrograms _ ([], [])          = putStr ""
runPrograms path (ps, os) = generateTestCases path (reverse ps, reverse os)

generateTestCases :: String -> ([String], [String]) -> IO ()
-- generateTestCases path ([], [])     = return $ TestList []
-- generateTestCases path (p:ps, o:os) = do 
--   TestList tests <- generateTestCases path (ps, os)
--   test <- generateTest (p, o)
--   return $ TestList (test : tests)
generateTestCases path ([], [])     = putStrLn $ "Done with: " ++ path
generateTestCases path (p:ps, o:os) = do 
  generateTest (p, o)
  generateTestCases path (ps, os)
generateTestCases path _ = error $ "Missing test file at: " ++ path

generateTest :: (String, String) -> IO ()
generateTest (file, output) = do 
  program <- readFile file 
  let Right result = evaluateString program
  e <- readFile output
  let expected = readValue e
  hspec $ describe ("Testing : " ++ file) $ it ("Program: " ++ program) $ result `shouldBe` expected
  where 
    readValue :: (Read a) => String -> Value a 
    readValue s = read s