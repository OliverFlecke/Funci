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
  expressionTests
  functionTests

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
      -- evaluateString "main = [1,2,3]" `shouldBe` (Listy (Cons (Number (I 1)) (Cons (Number (I 2)) (Cons (Number (I 3)) Empty))))
      evaluateString "main = 1.0 : 2.0 : 3.0 : []" `shouldBe` (Listy (Cons (Number (F 1.0)) (Cons (Number (F 2.0)) (Cons (Number (F 3.0)) Empty))))
      -- evaluateString "main = [1.0,2.0,3.0]" `shouldBe` (Listy (Cons (Number (F 1.0)) (Cons (Number (F 2.0)) (Cons (Number (F 3.0)) Empty))))
      evaluateString "main = 1 + 2 : []" `shouldBe` (Listy (Cons (Number (I 3)) Empty))
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

expressionTests = hspec $ do 
  describe "Testing let expressions -" $ do 
    it "Simple let" $ do
      evaluateString "main = let x = 1 in x" `shouldBe` (Number (I 1))
      evaluateString "main = let y = True in y" `shouldBe` (Boolean True)
    it "Let with arithmics" $ do 
      evaluateString "main = let x = 1 in x + 1" `shouldBe` (Number (I 2))
      evaluateString "main = let x = True in x || False" `shouldBe` (Boolean True)
    it "Let expressions with multiple variables" $ do 
      evaluateString "main = let x = 1, y = 2 in x + y" `shouldBe` (Number (I 3))
      evaluateString "main = let x = True, y = False in y && x" `shouldBe` (Boolean False)
    it "If then else expressions:" $ do 
      evaluateString "main = if True then 1 else 0" `shouldBe` (Number (I 1))
      evaluateString "main = if False then 1 else 0" `shouldBe` (Number (I 0))

functionTests = hspec $ do 
  describe "Testing functions without parameters -" $ do 
    it "Call function from main" $ do 
      evaluateString "main = f; f = 1" `shouldBe` (Number (I 1))  
    it "Calling function from function" $ do 
      evaluateString "main = f; f = g; g = True" `shouldBe` (Boolean True)
  describe "Testing functions with one parameter -" $ do 
    it "Calling function with a number" $ do 
      evaluateString "main = f 1; f x = x" `shouldBe` (Number (I 1))
    it "Callning functions with booleans" $ do 
      evaluateString "main = f True; f x = x" `shouldBe` (Boolean True)
      evaluateString "main = not True; not x = !x" `shouldBe` (Boolean False)
    it "Calling functions with expressions" $ do 
      evaluateString "main = f 1 + 2; f x = x + 1" `shouldBe` (Number (I 4)) 
  describe "Testing functions with multiple parameters -" $ do 
    it "Calling functions with multiple numbers" $ do 
      evaluateString "main = add 1 2; add x y = x + y" `shouldBe` (Number (I 3))