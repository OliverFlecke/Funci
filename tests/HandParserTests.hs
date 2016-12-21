module Main where

import HandSyntax
import HandLexer 
import HandParser 
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = do 
  testArithmics
  testBooleans
  testLetExpressions

-- Testing all the arithmic operations
testArithmics = hspec $ do 
  describe "Simple arithmic operations" $ do 
    it "is testing simple addition operations" $ do 
      parseArithmicString "2 + 3" `shouldBe` Right (App (App (Prim Add) (Const 2)) (Const 3))
      parseArithmicString "1 + 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (Const 1)) (Const 2))) (Const 3))
      parseArithmicString "1 + 2 + 3 + 4 + 5" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (Const 1)) (Const 2))) (Const 3))) (Const 4))) (Const 5))
    it "is testing simple multiplcation programs" $ do
      parseArithmicString "1 * 2" `shouldBe` Right (App (App (Prim Mul) (Const 1)) (Const 2))
      parseArithmicString "1 * 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (Const 1)) (Const 2))) (Const 3))
      parseArithmicString "1 * 2 * 3 * 4 * 5" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (Const 1)) (Const 2))) (Const 3))) (Const 4))) (Const 5))
    it "is testing interleaving addition and multiplcation operations" $ do 
      parseArithmicString "1 * 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const 1)) (Const 2))) (Const 3))
      parseArithmicString "1 + 2 * 3" `shouldBe` Right (App (App (Prim Add) (Const 1)) (App (App (Prim Mul) (Const 2)) (Const 3)))
      parseArithmicString "1 * 2 + 3 * 4" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const 1)) (Const 2))) (App (App (Prim Mul) (Const 3)) (Const 4)))
    it "is testing simple subtration operations" $ do 
      parseArithmicString "2 - 1" `shouldBe` Right (App (App (Prim Sub) (Const 2)) (Const 1))
      parseArithmicString "3 - 2 - 1" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Sub) (Const 3)) (Const 2))) (Const 1))
    it "is testing simple division operations" $ do 
      parseArithmicString "1 / 2" `shouldBe` Right (App (App (Prim Div) (Const 1)) (Const 2))
      parseArithmicString "1 / 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Div) (Const 1)) (Const 2))) (Const 3))
      parseArithmicString "1 / 0" `shouldBe` Left "Arithmic error: Divide by zero"
    it "is testing interleaving addition and subtration operations" $ do
      parseArithmicString "1 + 2 - 3" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Add) (Const 1)) (Const 2))) (Const 3))
      parseArithmicString "1 - 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Sub) (Const 1)) (Const 2))) (Const 3))
    it "is testing interleaving multiplcation and division operations" $ do 
      parseArithmicString "1 * 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Mul) (Const 1)) (Const 2))) (Const 3))
      parseArithmicString "1 / 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Div) (Const 1)) (Const 2))) (Const 3))
  describe "Arithmics with variables" $ do 
    it "Addition" $       parseArithmicString "x + y" `shouldBe` Right (App (App (Prim Add) (Var "x")) (Var "y"))
    it "Subtraction" $    parseArithmicString "x - y" `shouldBe` Right (App (App (Prim Sub) (Var "x")) (Var "y"))
    it "Multiplcation" $  parseArithmicString "x * y" `shouldBe` Right (App (App (Prim Mul) (Var "x")) (Var "y"))
    it "Division" $       parseArithmicString "x / y" `shouldBe` Right (App (App (Prim Div) (Var "x")) (Var "y"))

-- Testing booleans
testBooleans = hspec $ do 
  describe "Testing simple constants:" $ do 
    it "True" $ parseBooleanString "True" `shouldBe` Right (ConstBool True)
    it "False" $ parseBooleanString "False" `shouldBe` Right (ConstBool False)
    it "Variable" $ parseBooleanString "x" `shouldBe` Right (Var "x")
    it "Parentesics" $ parseBooleanString "(True)" `shouldBe` Right (ConstBool True)
  describe "Testing simple expressions:" $ do 
    it "And" $ parseBooleanString "True && False" `shouldBe` Right (App (App (Prim And) (ConstBool True)) (ConstBool False))
    it "Or" $ parseBooleanString "True || False" `shouldBe` Right (App (App (Prim Or) (ConstBool True)) (ConstBool False)) 
    it "Not" $ parseBooleanString "!True" `shouldBe` Right (App (Prim Not) (ConstBool True))
  describe "Testing mulitple operators after each other:" $ do
    it "Multiple Ands" $ parseBooleanString "True && False && True" `shouldBe` Right (App (App (Prim And) (App (App (Prim And) (ConstBool True)) (ConstBool False))) (ConstBool True))
    it "Multiple Ors" $ parseBooleanString "True || False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim Or) (ConstBool True)) (ConstBool False))) (ConstBool True))
  describe "Testing precedence with interleaving And, Or, and Not operators:" $ do 
    it "T && F || T" $ parseBooleanString "True && False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (ConstBool True)) (ConstBool False))) (ConstBool True))
    it "T || T && F" $ parseBooleanString "True || True && False" `shouldBe` Right (App (App (Prim Or) (ConstBool True)) (App (App (Prim And) (ConstBool True)) (ConstBool False)))
    it "-T && T" $ parseBooleanString "!True && True" `shouldBe` Right (App (App (Prim And) (App (Prim Not) (ConstBool True))) (ConstBool True))
    it "-F || F" $ parseBooleanString "!False || False" `shouldBe` Right (App (App (Prim Or) (App (Prim Not) (ConstBool False))) (ConstBool False))
    it "-F && T || F" $ parseBooleanString "!False && True || False" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (App (Prim Not) (ConstBool False))) (ConstBool True))) (ConstBool False))
    it "-(F && T)" $ parseBooleanString "!(False && True)" `shouldBe` Right (App (Prim Not) (App (App (Prim And) (ConstBool False)) (ConstBool True)))

testLetExpressions = hspec $ do
  describe "Testing let expressions" $ do 
    it "Single variable let expressions" $ do 
      parseString "let x = 1 in x" `shouldBe` Right (LetIn "x" (Const 1) (Var "x"))
      parseString "let x = 1 + 1 in x" `shouldBe` Right (LetIn "x" (App (App (Prim Add) (Const 1)) (Const 1)) (Var "x"))
      parseString "let x = 1 in x * x" `shouldBe` Right (LetIn "x" (Const 1) (App (App (Prim Mul) (Var "x")) (Var "x")))