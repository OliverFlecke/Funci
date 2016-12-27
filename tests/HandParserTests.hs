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
  testLet
  testIfThenElse
  testExpressions
  testMainFunction

-- Testing all the arithmic operations
testArithmics = hspec $ do 
  describe "Simple arithmic operations" $ do 
    it "is testing simple addition operations" $ do 
      parseArithmicString "2 + 3" `shouldBe` Right (App (App (Prim Add) (Const (Number (Integer 2)))) (Const (Number (Integer 3))))
      parseArithmicString "1 + 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
      parseArithmicString "1 + 2 + 3 + 4 + 5" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))) (Const (Number (Integer 4))))) (Const (Number (Integer 5))))
    it "is testing simple multiplcation programs" $ do
      parseArithmicString "1 * 2" `shouldBe` Right (App (App (Prim Mul) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))
      parseArithmicString "1 * 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
      parseArithmicString "1 * 2 * 3 * 4 * 5" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))) (Const (Number (Integer 4))))) (Const (Number (Integer 5))))
    it "is testing interleaving addition and multiplcation operations" $ do 
      parseArithmicString "1 * 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
      parseArithmicString "1 + 2 * 3" `shouldBe` Right (App (App (Prim Add) (Const (Number (Integer 1)))) (App (App (Prim Mul) (Const (Number (Integer 2)))) (Const (Number (Integer 3)))))
      parseArithmicString "1 * 2 + 3 * 4" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (App (App (Prim Mul) (Const (Number (Integer 3)))) (Const (Number (Integer 4)))))
    it "is testing simple subtration operations" $ do 
      parseArithmicString "2 - 1" `shouldBe` Right (App (App (Prim Sub) (Const (Number (Integer 2)))) (Const (Number (Integer 1))))
      parseArithmicString "3 - 2 - 1" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Sub) (Const (Number (Integer 3)))) (Const (Number (Integer 2))))) (Const (Number (Integer 1))))
    it "is testing simple division operations" $ do 
      parseArithmicString "1 / 2" `shouldBe` Right (App (App (Prim Div) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))
      parseArithmicString "1 / 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Div) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
      parseArithmicString "1 / 0" `shouldBe` Left "Arithmic error: Divide by zero"
    it "is testing interleaving addition and subtration operations" $ do
      parseArithmicString "1 + 2 - 3" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Add) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
      parseArithmicString "1 - 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Sub) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
    it "is testing interleaving multiplcation and division operations" $ do 
      parseArithmicString "1 * 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Mul) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
      parseArithmicString "1 / 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Div) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))) (Const (Number (Integer 3))))
  describe "Arithmics with variables" $ do 
    it "Addition" $       parseArithmicString "x + y" `shouldBe` Right (App (App (Prim Add) (Var "x")) (Var "y"))
    it "Subtraction" $    parseArithmicString "x - y" `shouldBe` Right (App (App (Prim Sub) (Var "x")) (Var "y"))
    it "Multiplcation" $  parseArithmicString "x * y" `shouldBe` Right (App (App (Prim Mul) (Var "x")) (Var "y"))
    it "Division" $       parseArithmicString "x / y" `shouldBe` Right (App (App (Prim Div) (Var "x")) (Var "y"))

-- Testing booleans
testBooleans = hspec $ do 
  describe "Testing simple constants:" $ do 
    it "True" $ parseBooleanString "True" `shouldBe` Right (Const (Boole True))
    it "False" $ parseBooleanString "False" `shouldBe` Right (Const (Boole False))
    it "Variable" $ parseBooleanString "x" `shouldBe` Right (Var "x")
    it "Parentesics" $ parseBooleanString "(True)" `shouldBe` Right (Const (Boole True))
  describe "Testing simple expressions:" $ do 
    it "And" $ parseBooleanString "True && False" `shouldBe` Right (App (App (Prim And) (Const (Boole True))) (Const (Boole False)))
    it "Or" $ parseBooleanString "True || False" `shouldBe` Right (App (App (Prim Or) (Const (Boole True))) (Const (Boole False))) 
    it "Not" $ parseBooleanString "!True" `shouldBe` Right (App (Prim Not) (Const (Boole True)))
  describe "Testing mulitple operators after each other:" $ do
    it "Multiple Ands" $ parseBooleanString "True && False && True" `shouldBe` Right (App (App (Prim And) (App (App (Prim And) (Const (Boole True))) (Const (Boole False)))) (Const (Boole True)))
    it "Multiple Ors" $ parseBooleanString "True || False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim Or) (Const (Boole True))) (Const (Boole False)))) (Const (Boole True)))
  describe "Testing precedence with interleaving And, Or, and Not operators:" $ do 
    it "T && F || T" $ parseBooleanString "True && False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (Const (Boole True))) (Const (Boole False)))) (Const (Boole True)))
    it "T || T && F" $ parseBooleanString "True || True && False" `shouldBe` Right (App (App (Prim Or) (Const (Boole True))) (App (App (Prim And) (Const (Boole True))) (Const (Boole False))))
    it "-T && T" $ parseBooleanString "!True && True" `shouldBe` Right (App (App (Prim And) (App (Prim Not) (Const (Boole True)))) (Const (Boole True)))
    it "-F || F" $ parseBooleanString "!False || False" `shouldBe` Right (App (App (Prim Or) (App (Prim Not) (Const (Boole False)))) (Const (Boole False)))
    it "-F && T || F" $ parseBooleanString "!False && True || False" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (App (Prim Not) (Const (Boole False)))) (Const (Boole True)))) (Const (Boole False)))
    it "-(F && T)" $ parseBooleanString "!(False && True)" `shouldBe` Right (App (Prim Not) (App (App (Prim And) (Const (Boole False))) (Const (Boole True))))

testLists = hspec $ do 
  describe "Testing simple list construction:" $ do
    it "Empty" $ parseListString "[]" `shouldBe` Right (Const (ConstList Empty))
    it "One int" $ parseListString "1 : []" `shouldBe` Right (Const (ConstList (Cons (Number (Integer 1)) Empty)))
    it "Two numbers" $ parseListString "1 : 2 : []" `shouldBe` Right (Const (ConstList (Cons (Number (Integer 1)) (Cons (Number (Integer 2)) Empty))))
    -- it "Invalid syntax with empty list in the middle" $ parseListString "1 : [] : 2" `shouldBe` Left "Parser error: Unable to construct list"
  describe "Testing list constructing with commas" $ do 
    it "Just one number" $ parseListString "[1]" `shouldBe` Right (Const (ConstList (Cons (Number (Integer 1)) Empty)))
    it "Three numbers" $ parseListString "[1,2,3]" `shouldBe` Right (Const (ConstList (Cons (Number (Integer 1)) (Cons (Number (Integer 2)) (Cons (Number (Integer 3) )Empty)))))
  -- describe "Testing list containing arithmic expressions:" $ do
  --   it "Simple addition" $ parseListString "1 + 2 : []" `shouldBe` Right (Const (ConstList (Cons (Number (Integer 3)) Empty)))
  describe "Testing list with booleans:" $ do 
    it "One boolean" $ parseListString "True : []" `shouldBe` Right (Const (ConstList (Cons (Boole True) Empty)))
    it "Mulitple booleans" $ parseListString "True : False : True : []" `shouldBe` Right (Const (ConstList (Cons (Boole True) (Cons (Boole False) (Cons (Boole True) Empty)))))
  -- describe "Testing list with different types:" $ do
  --   it "Int and bool" $ parseListString "True : 1 : []" `shouldBe` Left "Parser error: Lists cannot have multiple types"
    -- it "Int and bool" $ parseListString "True : 1 : []" `shouldBe` Right (Const (ConstList (Cons (Boole True) (Cons (Number (Int 1)) Empty))))

testLet = hspec $ do
  describe "Testing let expressions" $ do 
    it "Single variable let expressions with number" $ do 
      parseExpressionsString "let x = 1 in x" `shouldBe` Right (LetIn [("x", (Const (Number (Integer 1))))] (Var "x"))
      parseExpressionsString "let x = 1 + 1 in x" `shouldBe` Right (LetIn [("x", (App (App (Prim Add) (Const (Number (Integer 1)))) (Const (Number (Integer 1)))))] (Var "x"))
      parseExpressionsString "let x = 1 in x * x" `shouldBe` Right (LetIn [("x", (Const (Number (Integer 1))))] (App (App (Prim Mul) (Var "x")) (Var "x")))
    it "Single variable let expressions with booleans" $ do 
      parseExpressionsString "let x = True in x" `shouldBe` Right (LetIn [("x", (Const (Boole True)))] (Var "x"))
  describe "Multiple variables let expressions" $ do 
    it "Testing numbers and boolean" $ do 
      parseExpressionsString "let x = 1, y = 2 in x + y" `shouldBe` Right (LetIn [("x", (Const (Number (Integer 1)))), ("y", (Const (Number (Integer 2))))] (App (App (Prim Add) (Var "x")) (Var "y")))
      parseExpressionsString "let x = 1, y = True in y" `shouldBe` Right (LetIn [("x", (Const (Number (Integer 1)))), ("y", (Const (Boole True)))] (Var "y"))

testIfThenElse = hspec $ do 
  describe "Testing simple if then else expressions" $ do
    it "If true then 1 else 0" $ parseExpressionsString "if True then 1 else 0" `shouldBe` Right (IfThenElse (Const (Boole True)) (Const (Number (Integer 1))) (Const (Number (Integer 0))))
    it "If false then true else false" $ parseExpressionsString "if True then True else False" `shouldBe` Right (IfThenElse (Const (Boole True)) (Const (Boole True)) (Const (Boole False)))
  describe "Testing with more advance if expressions" $ do 
    it "if t || f then 1 else 0" $ parseExpressionsString "if True || False then 1 else 0" `shouldBe` Right (IfThenElse (App (App (Prim Or) (Const (Boole True))) (Const (Boole False))) (Const (Number (Integer 1))) (Const (Number (Integer 0))))

testExpressions = hspec $ do 
  describe "Testing interleaving if-then-else and let expressions" $ do 
    it "If t then let x = 1 in x else 0" $ parseExpressionsString "if True then let x = 1 in x else 0" `shouldBe` Right (IfThenElse (Const (Boole True)) (LetIn [("x", (Const (Number (Integer 1))))] (Var "x")) (Const (Number (Integer 0))))
    it "If t then 0 else let x = 1 in x" $ parseExpressionsString "if True then 0 else let x = 1 in x" `shouldBe` Right (IfThenElse (Const (Boole True)) (Const (Number (Integer 0))) (LetIn [("x", (Const (Number (Integer 1))))] (Var "x")))

testMainFunction = hspec $ do 
  describe "Testing a simple starting point for a program" $ do 
    it "Main function" $ parseString "main = 0" `shouldBe` Right [Bind "main" Nothing [] (Const (Number (Integer 0)))]
    it "Main with arithmic expression" $ parseString "main = 1 + 2" `shouldBe` Right [Bind "main" Nothing [] (App (App (Prim Add) (Const (Number (Integer 1)))) (Const (Number (Integer 2))))]
    it "Main with boolean" $ parseString "main = True" `shouldBe` Right [Bind "main" Nothing [] (Const (Boole True))] 
    it "Main with boolean expression" $ parseString "main = True || False" `shouldBe` Right [Bind "main" Nothing [] (App (App (Prim Or) (Const (Boole True))) (Const (Boole False)))]
  describe "Testing with expressions inside the main function" $ do 
    it "With let expression" $ parseString "main = let x = 1 in x" `shouldBe` Right [Bind "main" Nothing [] (LetIn [("x", (Const (Number (Integer 1))))] (Var "x"))]