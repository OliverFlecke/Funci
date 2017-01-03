module Main where

import Syntax
import Lexer 
import Parser 
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = do 
  -- testArithmics
  -- testBooleans
  -- comparatorOperatorsTests
  -- testLists
  -- testLet
  -- testIfThenElse
  -- testExpressions
  -- testMainFunction
  -- functionTests
  letFunctionTests

-- Testing all the arithmic operations
testArithmics = hspec $ do 
  describe "Simple arithmic operations with integers" $ do 
    it "is testing simple addition operations" $ do 
      parseArithmicString "2 + 3" `shouldBe` Right (App (App (Prim Add) (Const (Number (I 2)))) (Const (Number (I 3))))
      parseArithmicString "1 + 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
      parseArithmicString "1 + 2 + 3 + 4 + 5" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))) (Const (Number (I 4))))) (Const (Number (I 5))))
    it "is testing simple multiplcation programs" $ do
      parseArithmicString "1 * 2" `shouldBe` Right (App (App (Prim Mul) (Const (Number (I 1)))) (Const (Number (I 2))))
      parseArithmicString "1 * 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
      parseArithmicString "1 * 2 * 3 * 4 * 5" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))) (Const (Number (I 4))))) (Const (Number (I 5))))
    it "is testing interleaving addition and multiplcation operations" $ do 
      parseArithmicString "1 * 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
      parseArithmicString "1 + 2 * 3" `shouldBe` Right (App (App (Prim Add) (Const (Number (I 1)))) (App (App (Prim Mul) (Const (Number (I 2)))) (Const (Number (I 3)))))
      parseArithmicString "1 * 2 + 3 * 4" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (I 1)))) (Const (Number (I 2))))) (App (App (Prim Mul) (Const (Number (I 3)))) (Const (Number (I 4)))))
    it "is testing simple subtration operations" $ do 
      parseArithmicString "2 - 1" `shouldBe` Right (App (App (Prim Sub) (Const (Number (I 2)))) (Const (Number (I 1))))
      parseArithmicString "3 - 2 - 1" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Sub) (Const (Number (I 3)))) (Const (Number (I 2))))) (Const (Number (I 1))))
    it "is testing simple division operations" $ do 
      parseArithmicString "1 / 2" `shouldBe` Right (App (App (Prim Div) (Const (Number (I 1)))) (Const (Number (I 2))))
      parseArithmicString "1 / 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Div) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
      -- parseArithmicString "1 / 0" `shouldBe` Left "Arithmic error: Divide by zero"
    it "is testing interleaving addition and subtration operations" $ do
      parseArithmicString "1 + 2 - 3" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
      parseArithmicString "1 - 2 + 3" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Sub) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
    it "is testing interleaving multiplcation and division operations" $ do 
      parseArithmicString "1 * 2 / 3" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Mul) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
      parseArithmicString "1 / 2 * 3" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Div) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))
    it "Testing reminder operator" $ do 
      parseArithmicString "5 % 2" `shouldBe` Right (App (App (Prim Mod) (Const (Number (I 5)))) (Const (Number (I 2))))
      parseArithmicString "5 % 2 % 2" `shouldBe` Right (App (App (Prim Mod) (App (App (Prim Mod) (Const (Number (I 5)))) (Const (Number (I 2))))) (Const (Number (I 2))))
    it "Testing Parentesics" $ do 
      parseArithmicString "2 * (1 + 3)" `shouldBe` Right (App (App (Prim Mul) (Const (Number (I 2)))) (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 3)))))
  describe "Simple arithmic operations with floating points" $ do 
    it "is testing simple addition operations" $ do 
      parseArithmicString "2.0 + 3.0" `shouldBe` Right (App (App (Prim Add) (Const (Number (F 2.0)))) (Const (Number (F 3.0))))
      parseArithmicString "1.0 + 2.0 + 3.0" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
      parseArithmicString "1.0 + 2.0 + 3.0 + 4.0 + 5.0" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (App (App (Prim Add) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))) (Const (Number (F 4.0))))) (Const (Number (F 5.0))))
    it "is testing simple multiplcation programs" $ do
      parseArithmicString "1.0 * 2.0" `shouldBe` Right (App (App (Prim Mul) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))
      parseArithmicString "1.0 * 2.0 * 3.0" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
      parseArithmicString "1.0 * 2.0 * 3.0 * 4.0 * 5.0" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (App (App (Prim Mul) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))) (Const (Number (F 4.0))))) (Const (Number (F 5.0))))
    it "is testing interleaving addition and multiplcation operations" $ do 
      parseArithmicString "1.0 * 2.0 + 3.0" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
      parseArithmicString "1.0 + 2.0 * 3.0" `shouldBe` Right (App (App (Prim Add) (Const (Number (F 1.0)))) (App (App (Prim Mul) (Const (Number (F 2.0)))) (Const (Number (F 3.0)))))
      parseArithmicString "1.0 * 2.0 + 3.0 * 4.0" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Mul) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (App (App (Prim Mul) (Const (Number (F 3.0)))) (Const (Number (F 4.0)))))
    it "is testing simple subtration operations" $ do 
      parseArithmicString "2.0 - 1.0" `shouldBe` Right (App (App (Prim Sub) (Const (Number (F 2.0)))) (Const (Number (F 1.0))))
      parseArithmicString "3.0 - 2.0 - 1.0" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Sub) (Const (Number (F 3.0)))) (Const (Number (F 2.0))))) (Const (Number (F 1.0))))
    it "is testing simple division operations" $ do 
      parseArithmicString "1.0 / 2.0" `shouldBe` Right (App (App (Prim Div) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))
      parseArithmicString "1.0 / 2.0 / 3.0" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Div) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
      -- parseArithmicString "1.0 / 0" `shouldBe` Left "Arithmic error: Divide by zero"
    it "is testing interleaving addition and subtration operations" $ do
      parseArithmicString "1.0 + 2.0 - 3.0" `shouldBe` Right (App (App (Prim Sub) (App (App (Prim Add) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
      parseArithmicString "1.0 - 2.0 + 3.0" `shouldBe` Right (App (App (Prim Add) (App (App (Prim Sub) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
    it "is testing interleaving multiplcation and division operations" $ do 
      parseArithmicString "1.0 * 2.0 / 3.0" `shouldBe` Right (App (App (Prim Div) (App (App (Prim Mul) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
      parseArithmicString "1.0 / 2.0 * 3.0" `shouldBe` Right (App (App (Prim Mul) (App (App (Prim Div) (Const (Number (F 1.0)))) (Const (Number (F 2.0))))) (Const (Number (F 3.0))))
  describe "Arithmics with variables" $ do 
    it "Addition" $       parseArithmicString "x + y" `shouldBe` Right (App (App (Prim Add) (Var "x")) (Var "y"))
    it "Subtraction" $    parseArithmicString "x - y" `shouldBe` Right (App (App (Prim Sub) (Var "x")) (Var "y"))
    it "Multiplcation" $  parseArithmicString "x * y" `shouldBe` Right (App (App (Prim Mul) (Var "x")) (Var "y"))
    it "Division" $       parseArithmicString "x / y" `shouldBe` Right (App (App (Prim Div) (Var "x")) (Var "y"))
    it "Modulo" $       parseArithmicString "x % y" `shouldBe` Right (App (App (Prim Mod) (Var "x")) (Var "y"))

-- Testing booleans
testBooleans = hspec $ do 
  describe "Testing simple constants:" $ do 
    it "True" $ parseArithmicString "True" `shouldBe` Right (Const (Boolean True))
    it "False" $ parseArithmicString "False" `shouldBe` Right (Const (Boolean False))
    it "Variable" $ parseArithmicString "x" `shouldBe` Right (Var "x")
    it "Parentesics" $ parseArithmicString "(True)" `shouldBe` Right (Const (Boolean True))
  describe "Testing simple expressions:" $ do 
    it "And" $ parseArithmicString "True && False" `shouldBe` Right (App (App (Prim And) (Const (Boolean True))) (Const (Boolean False)))
    it "Or" $ parseArithmicString "True || False" `shouldBe` Right (App (App (Prim Or) (Const (Boolean True))) (Const (Boolean False))) 
    it "Not" $ parseArithmicString "!True" `shouldBe` Right (App (Prim Not) (Const (Boolean True)))
  describe "Testing mulitple operators after each other:" $ do
    it "Multiple Ands" $ parseArithmicString "True && False && True" `shouldBe` Right (App (App (Prim And) (App (App (Prim And) (Const (Boolean True))) (Const (Boolean False)))) (Const (Boolean True)))
    it "Multiple Ors" $ parseArithmicString "True || False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim Or) (Const (Boolean True))) (Const (Boolean False)))) (Const (Boolean True)))
  describe "Testing precedence with interleaving And, Or, and Not operators:" $ do 
    it "T && F || T" $ parseArithmicString "True && False || True" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (Const (Boolean True))) (Const (Boolean False)))) (Const (Boolean True)))
    it "T || T && F" $ parseArithmicString "True || True && False" `shouldBe` Right (App (App (Prim Or) (Const (Boolean True))) (App (App (Prim And) (Const (Boolean True))) (Const (Boolean False))))
    it "-T && T" $ parseArithmicString "!True && True" `shouldBe` Right (App (App (Prim And) (App (Prim Not) (Const (Boolean True)))) (Const (Boolean True)))
    it "-F || F" $ parseArithmicString "!False || False" `shouldBe` Right (App (App (Prim Or) (App (Prim Not) (Const (Boolean False)))) (Const (Boolean False)))
    it "-F && T || F" $ parseArithmicString "!False && True || False" `shouldBe` Right (App (App (Prim Or) (App (App (Prim And) (App (Prim Not) (Const (Boolean False)))) (Const (Boolean True)))) (Const (Boolean False)))
    it "-(F && T)" $ parseArithmicString "!(False && True)" `shouldBe` Right (App (Prim Not) (App (App (Prim And) (Const (Boolean False))) (Const (Boolean True))))

comparatorOperatorsTests = hspec $ do 
  describe "Testing comparator operators" $ do 
    it "Equality" $               parseArithmicString "1 == 2"  `shouldBe` Right (App (App (Prim Eq) (Const (Number (I 1)))) (Const (Number (I 2))))
    it "Inequality" $             parseArithmicString "1 != 2"  `shouldBe` Right (App (App (Prim Ne) (Const (Number (I 1)))) (Const (Number (I 2))))
    it "Greater than" $           parseArithmicString "1 > 2"   `shouldBe` Right (App (App (Prim Gt) (Const (Number (I 1)))) (Const (Number (I 2))))
    it "Less than" $              parseArithmicString "1 < 2"   `shouldBe` Right (App (App (Prim Lt) (Const (Number (I 1)))) (Const (Number (I 2))))
    it "Greater than or equal" $  parseArithmicString "1 >= 2"  `shouldBe` Right (App (App (Prim Ge) (Const (Number (I 1)))) (Const (Number (I 2))))
    it "Less than or equal" $     parseArithmicString "1 <= 2"  `shouldBe` Right (App (App (Prim Le) (Const (Number (I 1)))) (Const (Number (I 2))))

testLists = hspec $ do 
  describe "Testing simple list construction:" $ do
    it "Empty" $ parseArithmicString "[]" `shouldBe` Right (Const (Listy Empty))
    it "One int" $ parseArithmicString "1 : []" `shouldBe` Right (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Number (I 1)))) --Right (App (App (Prim ListCons) (Const (Number (I 1)))) (Const (Listy Empty)))
    it "Two numbers" $ parseArithmicString "1 : 2 : []" `shouldBe` Right (App (App (Prim ListCons) (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Number (I 2))))) (Const (Number (I 1)))) --Right (App (App (Prim ListCons) (App (App (Prim ListCons) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Listy Empty))) --Right (Const (ConstList (Cons (Number (I 1)) (Cons (Number (I 2)) Empty))))
  -- describe "Testing list constructing with sugar syntax" $ do 
  --   it "Just one number" $ parseArithmicString "[1]" `shouldBe` Right (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Number (I 1)))) --Right (Const (ConstList (Cons (Number (I 1)) Empty)))
  --   it "Two numbers [1:2]" $ parseArithmicString "[1:2]" `shouldBe` Right (App (App (Prim ListCons) (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Number (I 1))))) (Const (Number (I 2))))
  --   it "Three numbers [1:2:3]" $ parseArithmicString "[1:2:3]" `shouldBe` Right (App (App (Prim ListCons) (App (App (Prim ListCons) (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Number (I 3))))) (Const (Number (I 2))))) (Const (Number (I 1)))) --(App (App (Prim ListCons) (App (App (Prim ListCons) (App (App (Prim ListCons) (Const (Number (I 1)))) (Const (Number (I 2))))) (Const (Number (I 3))))) (Const (Listy Empty))) --Right (Const (ConstList (Cons (Number (I 1)) (Cons (Number (I 2)) (Cons (Number (I 3) )Empty)))))
  describe "Testing list containing arithmic expressions:" $ do
    it "Simple addition" $ parseArithmicString "1 + 2 : []" `shouldBe` Right (App (App (Prim ListCons) (Const (Listy Empty))) (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 2)))))
  describe "Testing list with booleans:" $ do 
    it "One boolean" $ parseArithmicString "True : []" `shouldBe` Right (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Boolean True))) --Right (Const (ConstList (Cons (Boolean True) Empty)))
    it "Mulitple booleans" $ parseArithmicString "True : False : True : []" `shouldBe` Right (App (App (Prim ListCons) (App (App (Prim ListCons) (App (App (Prim ListCons) (Const (Listy Empty))) (Const (Boolean True)))) (Const (Boolean False)))) (Const (Boolean True)))  --Right (Const (ConstList (Cons (Boolean True) (Cons (Boolean False) (Cons (Boolean True) Empty)))))
  -- describe "Testing list with different types:" $ do
  --   it "Int and bool" $ parseArithmicString "True : 1 : []" `shouldBe` Left "Parser error: Lists cannot have multiple types"
    -- it "Int and bool" $ parseArithmicString "True : 1 : []" `shouldBe` Right (Const (ConstList (Cons (Boolean True) (Cons (Number (Int 1)) Empty))))

testLet = hspec $ do
  describe "Testing let expressions" $ do 
    it "Single variable let expressions with number" $ do 
      parseExpressionsString "let x = 1 in x" `shouldBe` Right (LetIn [Bind "x" Nothing [] (Const (Number (I 1)))] (Var "x"))
      parseExpressionsString "let x = 1 + 1 in x" `shouldBe` Right (LetIn [Bind "x" Nothing [] (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 1))))] (Var "x"))
      parseExpressionsString "let x = 1 in x * x" `shouldBe` Right (LetIn [Bind "x" Nothing [] (Const (Number (I 1)))] (App (App (Prim Mul) (Var "x")) (Var "x")))
    it "Single variable let expressions with booleans" $ do 
      parseExpressionsString "let x = True in x" `shouldBe` Right (LetIn [Bind "x" Nothing [] (Const (Boolean True))] (Var "x"))
  describe "Multiple variables let expressions" $ do 
    it "Testing numbers and boolean" $ do 
      parseExpressionsString "let x = 1, y = 2 in x + y" `shouldBe` Right (LetIn [Bind "x" Nothing [] (Const (Number (I 1))), Bind "y" Nothing [] (Const (Number (I 2)))] (App (App (Prim Add) (Var "x")) (Var "y")))
      parseExpressionsString "let x = 1, y = True in y" `shouldBe` Right (LetIn [Bind "x" Nothing [] (Const (Number (I 1))), Bind "y" Nothing [] (Const (Boolean True))] (Var "y"))
      parseExpressionsString "let x = 1, y = True in y && True" `shouldBe` Right (LetIn [Bind "x" Nothing [] (Const (Number (I 1))), Bind "y" Nothing [] (Const (Boolean True))] (App (App (Prim And) (Var "y")) (Const (Boolean True))))
  describe "Parsing full programs with let expressions" $ do 
    it "Multiple variables" $ do
      parseString "main = let x = 1, y = True in y && True" `shouldBe` Right [Bind "main" Nothing [] (LetIn [Bind "x" Nothing [] (Const (Number (I 1))), Bind "y" Nothing [] (Const (Boolean True))] (App (App (Prim And) (Var "y")) (Const (Boolean True))))]

testIfThenElse = hspec $ do 
  describe "Testing simple if then else expressions" $ do
    it "If true then 1 else 0" $ parseExpressionsString "if True then 1 else 0" `shouldBe` Right (IfThenElse (Const (Boolean True)) (Const (Number (I 1))) (Const (Number (I 0))))
    it "If false then true else false" $ parseExpressionsString "if True then True else False" `shouldBe` Right (IfThenElse (Const (Boolean True)) (Const (Boolean True)) (Const (Boolean False)))
  describe "Testing with more advance if expressions" $ do 
    it "if t || f then 1 else 0" $ parseExpressionsString "if True || False then 1 else 0" `shouldBe` Right (IfThenElse (App (App (Prim Or) (Const (Boolean True))) (Const (Boolean False))) (Const (Number (I 1))) (Const (Number (I 0))))

testExpressions = hspec $ do 
  describe "Testing interleaving if-then-else and let expressions" $ do 
    it "If t then let x = 1 in x else 0" $ parseExpressionsString "if True then let x = 1 in x else 0" `shouldBe` Right (IfThenElse (Const (Boolean True)) (LetIn [Bind "x" Nothing [] (Const (Number (I 1)))] (Var "x")) (Const (Number (I 0))))
    it "If t then 0 else let x = 1 in x" $ parseExpressionsString "if True then 0 else let x = 1 in x" `shouldBe` Right (IfThenElse (Const (Boolean True)) (Const (Number (I 0))) (LetIn [Bind "x" Nothing [] (Const (Number (I 1)))] (Var "x")))

testMainFunction = hspec $ do 
  describe "Testing a simple starting point for a program" $ do 
    it "Main function" $ parseString "main = 0" `shouldBe` Right [Bind "main" Nothing [] (Const (Number (I 0)))]
    it "Main with arithmic expression" $ parseString "main = 1 + 2" `shouldBe` Right [Bind "main" Nothing [] (App (App (Prim Add) (Const (Number (I 1)))) (Const (Number (I 2))))]
    it "Main with boolean" $ parseString "main = True" `shouldBe` Right [Bind "main" Nothing [] (Const (Boolean True))] 
    it "Main with boolean expression" $ parseString "main = True || False" `shouldBe` Right [Bind "main" Nothing [] (App (App (Prim Or) (Const (Boolean True))) (Const (Boolean False)))]
  describe "Testing with expressions inside the main function" $ do 
    it "With let expression" $ parseString "main = let x = 1 in x" `shouldBe` Right [Bind "main" Nothing [] (LetIn [Bind "x" Nothing [] (Const (Number (I 1)))] (Var "x"))]

functionTests = hspec $ do 
  describe "Testing functions without variables" $ do 
    it "No variables" $ do 
      parseString "main = f; f = 1" `shouldBe` Right [Bind "main" Nothing [] (Var "f"), Bind "f" Nothing [] (Const (Number (I 1)))]
  describe "Testing writing functions with one variable" $ do 
    it "Calling a second function from main" $ do
      parseString "main = f 1; f x = x" `shouldBe` Right [Bind "main" Nothing [] (App (Var "f") (Const (Number (I 1)))), Bind "f" Nothing ["x"] (Var "x")]
      parseString "main = f True; f x = False" `shouldBe` Right [Bind "main" Nothing [] (App (Var "f") (Const (Boolean True))), Bind "f" Nothing ["x"] (Const (Boolean False))]
  describe "Testing parsing of mulitple parameters" $ do 
    it "Two parameters" $ parseString "main = add 1 2; add x y = x + y" `shouldBe` Right [Bind "main" Nothing [] (App (App (Var "add") (Const (Number (I 1)))) (Const (Number (I 2)))), Bind "add" Nothing ["x","y"] (App (App (Prim Add) (Var "x")) (Var "y"))]

letFunctionTests = hspec $ do 
  describe "Creating functions with let -" $ do 
    it "Let with function" $ parseExpressionsString "let f x = x in f 1" `shouldBe` Right (LetIn [Bind "f" Nothing ["x"] (Var "x")] (App (Var "f") (Const (Number (I 1)))))
    it "Let function with two arguments" $ parseExpressionsString "let add x y = x + y in add 1 2" `shouldBe` Right (LetIn [Bind "add" Nothing ["x", "y"] (App (App (Prim Add) (Var "x")) (Var "y"))] (App (App (Var "add") (Const (Number (I 1)))) (Const (Number (I 2)))))