module Main where

-- import Lexer
-- import Syntax
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
    it "evaluate integer" $ evaluateString "main = 0" `shouldBe` (I 0)
    it "evaluate float" $ evaluateString "main = 0.0" `shouldBe` (F 0.0)
    it "evaluate boolean" $ do 
      evaluateString "main = True" `shouldBe` (B True)
      evaluateString "main = False" `shouldBe` (B False)

arithmicTests = hspec $ do 
  describe "Evaluate numbering arithmics:" $ do 
    it "Addition" $ do 
      evaluateString "main = 1 + 2" `shouldBe` (I 3)
      evaluateString "main = 1.1 + 2.5" `shouldBe` (F 3.6)
    it "Subtraction" $ do 
      evaluateString "main = 2 - 1" `shouldBe` (I 1)
      evaluateString "main = 2.5 - 0.5" `shouldBe` (F 2.0)
    it "Multiplication" $ do 
      evaluateString "main = 2 * 3" `shouldBe` (I 6)
      evaluateString "main = 2.0 * 3.5" `shouldBe` (F 7)
    it "Divition" $ do
     evaluateString "main = 6 / 2" `shouldBe` (I 3)
     evaluateString "main = 6.0 / 2.0" `shouldBe` (F 3.0)
    it "Reminder" $ do 
      evaluateString "main = 5 % 2" `shouldBe` (I 1)

booleanArithmicTests = hspec $ do 
  describe "Evaluate boolean expressions" $ do 
    it "Not operator" $ do 
      evaluateString "main = !True" `shouldBe` (B False)
      evaluateString "main = !False" `shouldBe` (B True)
    it "And operator" $ do 
      evaluateString "main = True && True" `shouldBe` (B True)
      evaluateString "main = True && False" `shouldBe` (B False)
      evaluateString "main = False && True" `shouldBe` (B False)
      evaluateString "main = False && False" `shouldBe` (B False)
    it "Or operator" $ do 
      evaluateString "main = True || True" `shouldBe` (B True)
      evaluateString "main = True || False" `shouldBe` (B True)
      evaluateString "main = False || True" `shouldBe` (B True)
      evaluateString "main = False || False" `shouldBe` (B False)
    it "Interleaving operators" $ do 
      evaluateString "main = True && False || True" `shouldBe` (B True)
      evaluateString "main = False || False && True" `shouldBe` (B False)
      evaluateString "main = !False && True" `shouldBe` (B True)

compareOperatorTests = hspec $ do 
  describe "Evaluate comparator operators - " $ do 
    it "Testing equality operator" $ do 
      evaluateString "main = 1 == 1" `shouldBe` (B True)
      evaluateString "main = 1 == 0" `shouldBe` (B False)
      evaluateString "main = 1.0 == 1.0" `shouldBe` (B True)
      evaluateString "main = 1.0 == 0.0" `shouldBe` (B False)
    it "Testing inequality operator" $ do 
      evaluateString "main = 1 != 1" `shouldBe` (B False)
      evaluateString "main = 1 != 0" `shouldBe` (B True)
      evaluateString "main = 1.0 != 1.0" `shouldBe` (B False)
      evaluateString "main = 1.0 != 0.0" `shouldBe` (B True)
    it "Testing Greater than" $ do 
      evaluateString "main = 1 > 0" `shouldBe` (B True)
      evaluateString "main = 0 > 1" `shouldBe` (B False)
      evaluateString "main = 1.0 > 0.0" `shouldBe` (B True)
      evaluateString "main = 0.0 > 1.0" `shouldBe` (B False)    
    it "Testing Less than" $ do 
      evaluateString "main = 1 < 0" `shouldBe` (B False)
      evaluateString "main = 0 < 1" `shouldBe` (B True)
      evaluateString "main = 1.0 < 0.0" `shouldBe` (B False)
      evaluateString "main = 0.0 < 1.0" `shouldBe` (B True)
    it "Testing Greater than or equal to" $ do 
      evaluateString "main = 1 >= 0" `shouldBe` (B True)
      evaluateString "main = 0 >= 1" `shouldBe` (B False)
      evaluateString "main = 1 >= 1" `shouldBe` (B True)
      evaluateString "main = 1.0 >= 0.0" `shouldBe` (B True)
      evaluateString "main = 0.0 >= 1.0" `shouldBe` (B False)
      evaluateString "main = 1.0 >= 1.0" `shouldBe` (B True)
    it "Testing Less than or equal to" $ do 
      evaluateString "main = 1 <= 0" `shouldBe` (B False)
      evaluateString "main = 0 <= 1" `shouldBe` (B True)
      evaluateString "main = 1 <= 1" `shouldBe` (B True)
      evaluateString "main = 1.0 <= 0.0" `shouldBe` (B False)
      evaluateString "main = 0.0 <= 1.0" `shouldBe` (B True)
      evaluateString "main = 1.0 <= 1.0" `shouldBe` (B True)


