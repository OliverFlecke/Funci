module Main where

import Syntax
import Evaluator
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO () 
main = do 
  basicTests
  arithmicTests
  booleanArithmicTests
  compareOperatorTests

basicTests = hspec $ do 
  describe "Evaluate basics" $ do 
    it "evaluate integer" $ evaluateString "main = 0" `shouldBe` (Number (I 0))
    it "evaluate float" $ evaluateString "main = 0.0" `shouldBe` (Number (F 0.0))
    it "evaluate boolean" $ do 
      evaluateString "main = True" `shouldBe` (Boolean True)
      evaluateString "main = False" `shouldBe` (Boolean False)
  describe "Basic lists" $ do 
    it "Empty list" $ evaluateString "main = []" `shouldBe` (Listy Empty)
    it "List of integers" $ do
      evaluateString "main = 1 : []" `shouldBe` (Listy (Cons (Number (I 1)) Empty))
      evaluateString "main = 1 : 2 : []" `shouldBe` (Listy (Cons (Number (I 1)) (Cons (Number (I 2)) Empty)))
      evaluateString "main = 1 : 2 : 3 : []" `shouldBe` (Listy (Cons (Number (I 1)) (Cons (Number (I 2)) (Cons (Number (I 3)) Empty))))
      evaluateString "main = [1,2,3]" `shouldBe` (Listy (Cons (Number (I 1)) (Cons (Number (I 2)) (Cons (Number (I 3)) Empty))))
      evaluateString "main = 1.0 : 2.0 : 3.0 : []" `shouldBe` (Listy (Cons (Number (F 1.0)) (Cons (Number (F 2.0)) (Cons (Number (F 3.0)) Empty))))
      evaluateString "main = [1.0,2.0,3.0]" `shouldBe` (Listy (Cons (Number (F 1.0)) (Cons (Number (F 2.0)) (Cons (Number (F 3.0)) Empty))))
    it "List of booleans" $ do 
      evaluateString "main = True : False : True : []" `shouldBe` (Listy (Cons (Boolean True) (Cons (Boolean False) (Cons (Boolean True) Empty))))

arithmicTests = hspec $ do 
  describe "Evaluate numbering arithmics:" $ do 
    it "Addition" $ do 
      evaluateString "main = 1 + 2" `shouldBe` (Number (I 3))
      evaluateString "main = 1.1 + 2.5" `shouldBe` (Number (F 3.6))
    it "Subtraction" $ do 
      evaluateString "main = 2 - 1" `shouldBe` (Number (I 1))
      evaluateString "main = 2.5 - 0.5" `shouldBe` (Number (F 2.0))
    it "Multiplication" $ do 
      evaluateString "main = 2 * 3" `shouldBe` (Number (I 6))
      evaluateString "main = 2.0 * 3.5" `shouldBe` (Number (F 7.0))
    it "Divition" $ do
     evaluateString "main = 6 / 2" `shouldBe` (Number (I 3))
     evaluateString "main = 6.0 / 2.0" `shouldBe` (Number (F 3.0))
    it "Reminder" $ do 
      evaluateString "main = 5 % 2" `shouldBe` (Number (I 1))

booleanArithmicTests = hspec $ do 
  describe "Evaluate boolean expressions" $ do 
    it "Not operator" $ do 
      evaluateString "main = !True" `shouldBe` (Boolean False)
      evaluateString "main = !False" `shouldBe` (Boolean True)
    it "And operator" $ do 
      evaluateString "main = True && True" `shouldBe` (Boolean True)
      evaluateString "main = True && False" `shouldBe` (Boolean False)
      evaluateString "main = False && True" `shouldBe` (Boolean False)
      evaluateString "main = False && False" `shouldBe` (Boolean False)
    it "Or operator" $ do 
      evaluateString "main = True || True" `shouldBe` (Boolean True)
      evaluateString "main = True || False" `shouldBe` (Boolean True)
      evaluateString "main = False || True" `shouldBe` (Boolean True)
      evaluateString "main = False || False" `shouldBe` (Boolean False)
    it "Interleaving operators" $ do 
      evaluateString "main = True && False || True" `shouldBe` (Boolean True)
      evaluateString "main = False || False && True" `shouldBe` (Boolean False)
      evaluateString "main = !False && True" `shouldBe` (Boolean True)

compareOperatorTests = hspec $ do 
  describe "Evaluate comparator operators - " $ do 
    it "Testing equality operator" $ do 
      evaluateString "main = 1 == 1" `shouldBe` (Boolean True)
      evaluateString "main = 1 == 0" `shouldBe` (Boolean False)
      evaluateString "main = 1.0 == 1.0" `shouldBe` (Boolean True)
      evaluateString "main = 1.0 == 0.0" `shouldBe` (Boolean False)
    it "Testing inequality operator" $ do 
      evaluateString "main = 1 != 1" `shouldBe` (Boolean False)
      evaluateString "main = 1 != 0" `shouldBe` (Boolean True)
      evaluateString "main = 1.0 != 1.0" `shouldBe` (Boolean False)
      evaluateString "main = 1.0 != 0.0" `shouldBe` (Boolean True)
    it "Testing Greater than" $ do 
      evaluateString "main = 1 > 0" `shouldBe` (Boolean True)
      evaluateString "main = 0 > 1" `shouldBe` (Boolean False)
      evaluateString "main = 1.0 > 0.0" `shouldBe` (Boolean True)
      evaluateString "main = 0.0 > 1.0" `shouldBe` (Boolean False)    
    it "Testing Less than" $ do 
      evaluateString "main = 1 < 0" `shouldBe` (Boolean False)
      evaluateString "main = 0 < 1" `shouldBe` (Boolean True)
      evaluateString "main = 1.0 < 0.0" `shouldBe` (Boolean False)
      evaluateString "main = 0.0 < 1.0" `shouldBe` (Boolean True)
    it "Testing Greater than or equal to" $ do 
      evaluateString "main = 1 >= 0" `shouldBe` (Boolean True)
      evaluateString "main = 0 >= 1" `shouldBe` (Boolean False)
      evaluateString "main = 1 >= 1" `shouldBe` (Boolean True)
      evaluateString "main = 1.0 >= 0.0" `shouldBe` (Boolean True)
      evaluateString "main = 0.0 >= 1.0" `shouldBe` (Boolean False)
      evaluateString "main = 1.0 >= 1.0" `shouldBe` (Boolean True)
    it "Testing Less than or equal to" $ do 
      evaluateString "main = 1 <= 0" `shouldBe` (Boolean False)
      evaluateString "main = 0 <= 1" `shouldBe` (Boolean True)
      evaluateString "main = 1 <= 1" `shouldBe` (Boolean True)
      evaluateString "main = 1.0 <= 0.0" `shouldBe` (Boolean False)
      evaluateString "main = 0.0 <= 1.0" `shouldBe` (Boolean True)
      evaluateString "main = 1.0 <= 1.0" `shouldBe` (Boolean True)


