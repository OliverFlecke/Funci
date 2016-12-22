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
  testLists
  testLetExpressions

-- Testing all the arithmic operations
testArithmics = hspec $ do 
  describe "Simple arithmic operations" $ do 
    it "is testing simple addition operations" $ do 
      parseArithmicString "2 + 3" `shouldBe` Right (App (App (Prim Add) (Const (Number (Int 2)))) (Const (Number (Int 3))))
      parseArithmicString "1 + 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
      parseArithmicString "1 + 2 + 3 + 4 + 5" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))) (Const (Number (Int 4))))) (Const (Number (Int 5))))
    it "is testing simple multiplcation programs" $ do
      parseArithmicString "1 * 2" `shouldBe` Right (App (App (Prim Mul) (Const (Number (Int 1)))) (Const (Number (Int 2))))
      parseArithmicString "1 * 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
      parseArithmicString "1 * 2 * 3 * 4 * 5" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))) (Const (Number (Int 4))))) (Const (Number (Int 5))))
    it "is testing interleaving addition and multiplcation operations" $ do 
      parseArithmicString "1 * 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
      parseArithmicString "1 + 2 * 3" `shouldBe` Right (App (App (Prim Add) (Const (Number (Int 1)))) (App (App (Prim Mul) (Const (Number (Int 2)))) (Const (Number (Int 3)))))
      parseArithmicString "1 * 2 + 3 * 4" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (App (App (Prim Mul) (Const (Number (Int 3)))) (Const (Number (Int 4)))))
    it "is testing simple subtration operations" $ do 
      parseArithmicString "2 - 1" `shouldBe` Right (App (App (Prim Sub) (Const (Number (Int 2)))) (Const (Number (Int 1))))
      parseArithmicString "3 - 2 - 1" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Sub) (Const (Number (Int 3)))) (Const (Number (Int 2))))) (Const (Number (Int 1))))
    it "is testing simple division operations" $ do 
      parseArithmicString "1 / 2" `shouldBe` Right (App (App (Prim Div) (Const (Number (Int 1)))) (Const (Number (Int 2))))
      parseArithmicString "1 / 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Div) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
      parseArithmicString "1 / 0" `shouldBe` Left "Arithmic error: Divide by zero"
    it "is testing interleaving addition and subtration operations" $ do
      parseArithmicString "1 + 2 - 3" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Add) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
      parseArithmicString "1 - 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Sub) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
    it "is testing interleaving multiplcation and division operations" $ do 
      parseArithmicString "1 * 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Mul) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
      parseArithmicString "1 / 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Div) (Const (Number (Int 1)))) (Const (Number (Int 2))))) (Const (Number (Int 3))))
  describe "Arithmics with variables" $ do 
    it "Addition" $       parseArithmicString "x + y" `shouldBe` Right (App (App (Prim Add) (Var "x")) (Var "y"))
    it "Subtraction" $    parseArithmicString "x - y" `shouldBe` Right (App (App (Prim Sub) (Var "x")) (Var "y"))
    it "Multiplcation" $  parseArithmicString "x * y" `shouldBe` Right (App (App (Prim Mul) (Var "x")) (Var "y"))
    it "Division" $       parseArithmicString "x / y" `shouldBe` Right (App (App (Prim Div) (Var "x")) (Var "y"))

-- Testing booleans
testBooleans = hspec $ do 
  describe "Testing simple constants:" $ do 
    it "True" $ parseBooleanString "True" `shouldBe` Right (Const (Bool True))
    it "False" $ parseBooleanString "False" `shouldBe` Right (Const (Bool False))
    it "Variable" $ parseBooleanString "x" `shouldBe` Right (Var "x")
    it "Parentesics" $ parseBooleanString "(True)" `shouldBe` Right (Const (Bool True))
  describe "Testing simple expressions:" $ do 
    it "And" $ parseBooleanString "True && False" `shouldBe` Right (App (App (Prim And) (Const (Bool True))) (Const (Bool False)))
    it "Or" $ parseBooleanString "True || False" `shouldBe` Right (App (App (Prim Or) (Const (Bool True))) (Const (Bool False))) 
    it "Not" $ parseBooleanString "!True" `shouldBe` Right (App (Prim Not) (Const (Bool True)))
  describe "Testing mulitple operators after each other:" $ do
    it "Multiple Ands" $ parseBooleanString "True && False && True" `shouldBe` Right (App (App (Prim And) (App (App (Prim And) (Const (Bool True))) (Const (Bool False)))) (Const (Bool True)))
    it "Multiple Ors" $ parseBooleanString "True || False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim Or) (Const (Bool True))) (Const (Bool False)))) (Const (Bool True)))
  describe "Testing precedence with interleaving And, Or, and Not operators:" $ do 
    it "T && F || T" $ parseBooleanString "True && False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (Const (Bool True))) (Const (Bool False)))) (Const (Bool True)))
    it "T || T && F" $ parseBooleanString "True || True && False" `shouldBe` Right (App (App (Prim Or) (Const (Bool True))) (App (App (Prim And) (Const (Bool True))) (Const (Bool False))))
    it "-T && T" $ parseBooleanString "!True && True" `shouldBe` Right (App (App (Prim And) (App (Prim Not) (Const (Bool True)))) (Const (Bool True)))
    it "-F || F" $ parseBooleanString "!False || False" `shouldBe` Right (App (App (Prim Or) (App (Prim Not) (Const (Bool False)))) (Const (Bool False)))
    it "-F && T || F" $ parseBooleanString "!False && True || False" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (App (Prim Not) (Const (Bool False)))) (Const (Bool True)))) (Const (Bool False)))
    it "-(F && T)" $ parseBooleanString "!(False && True)" `shouldBe` Right (App (Prim Not) (App (App (Prim And) (Const (Bool False))) (Const (Bool True))))

testLists = hspec $ do 
  describe "Testing simple list construction:" $ do
    it "Empty" $ parseListString "[]" `shouldBe` Right (Const (ConstList Empty))
    it "One int" $ parseListString "1 : []" `shouldBe` Right (Const (ConstList (Cons (Number (Int 1)) Empty)))
    it "Two numbers" $ parseListString "1 : 2 : []" `shouldBe` Right (Const (ConstList (Cons (Number (Int 1)) (Cons (Number (Int 2)) Empty))))
    it "Invalid syntax with empty list in the middle" $ parseListString "1 : [] : 2" `shouldBe` Left "Parser error: Unable to construct list"
  describe "Testing list constructing with commas" $ do 
    it "Just one number" $ parseListString "[1]" `shouldBe` Right (Const (ConstList (Cons (Number (Int 1)) Empty)))
    it "Three numbers" $ parseListString "[1,2,3]" `shouldBe` Right (Const (ConstList (Cons (Number (Int 1)) (Cons (Number (Int 2)) (Cons (Number (Int 3) )Empty)))))
  describe "Testing list containing arithmic expressions:" $ do
    it "Simple addition" $ parseListString "1 + 2 : []" `shouldBe` Right (Const (ConstList (Cons (Number (Int 3)) Empty)))
  describe "Testing list with booleans:" $ do 
    it "One boolean" $ parseListString "True : []" `shouldBe` Right (Const (ConstList (Cons (Bool True) Empty)))
    it "Mulitple booleans" $ parseListString "True : False : True : []" `shouldBe` Right (Const (ConstList (Cons (Bool True) (Cons (Bool False) (Cons (Bool True) Empty)))))
  describe "Testing list with different types:" $ do
    it "Int and bool" $ parseListString "True : 1 : []" `shouldBe` Right (Const (ConstList (Cons (Bool True) (Cons (Number (Int 1)) Empty))))

testLetExpressions = hspec $ do
  describe "Testing let expressions" $ do 
    it "Single variable let expressions" $ do 
      parseString "let x = 1 in x" `shouldBe` Right (LetIn "x" (Const (Number (Int 1))) (Var "x"))
      parseString "let x = 1 + 1 in x" `shouldBe` Right (LetIn "x" (App (App (Prim Add) (Const (Number (Int 1)))) (Const (Number (Int 1)))) (Var "x"))
      parseString "let x = 1 in x * x" `shouldBe` Right (LetIn "x" (Const (Number (Int 1))) (App (App (Prim Mul) (Var "x")) (Var "x")))