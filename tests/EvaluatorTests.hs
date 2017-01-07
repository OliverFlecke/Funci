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
  evaluatingNumbersWithUnits

basicTests = hspec $ do 
  describe "Evaluate basics" $ do 
    it "evaluate integer" $ evaluateString "main = 0" `shouldBe` (Number 0 (Unit []))
    it "evaluate float" $ evaluateString "main = 0.0" `shouldBe` (Number 0.0 (Unit []))
    it "Negative number" $ evaluateString "main = -10" `shouldBe` (Number (-10) (Unit []))
    it "evaluate boolean" $ do 
      evaluateString "main = True" `shouldBe` (Boolean True)
      evaluateString "main = False" `shouldBe` (Boolean False)
  describe "Basic lists" $ do 
    it "Empty list" $ evaluateString "main = []" `shouldBe` (Listy Empty)
    it "List of integers" $ do
      evaluateString "main = 1 : []" `shouldBe` (Listy (Cons (Number 1 (Unit [])) Empty))
      evaluateString "main = 1 : 2 : []" `shouldBe` (Listy (Cons (Number 1 (Unit [])) (Cons (Number 2 (Unit [])) Empty)))
      evaluateString "main = 1 : 2 : 3 : []" `shouldBe` (Listy (Cons (Number 1 (Unit [])) (Cons (Number 2 (Unit [])) (Cons (Number 3 (Unit [])) Empty))))
      -- evaluateString "main = [1,2,3]" `shouldBe` (Listy (Cons (Number 1 (Unit [])) (Cons (Number 2 (Unit [])) (Cons (Number 3 (Unit [])) Empty))))
      evaluateString "main = 1.0 : 2.0 : 3.0 : []" `shouldBe` (Listy (Cons (Number 1.0 (Unit [])) (Cons (Number 2.0 (Unit [])) (Cons (Number 3.0 (Unit [])) Empty))))
      -- evaluateString "main = [1.0,2.0,3.0]" `shouldBe` (Listy (Cons (Number 1.0 (Unit [])) (Cons (Number 2.0 (Unit [])) (Cons (Number 3.0 (Unit [])) Empty))))
      evaluateString "main = 1 + 2 : []" `shouldBe` (Listy (Cons (Number 3 (Unit [])) Empty))
    it "List of booleans" $ do 
      evaluateString "main = True : False : True : []" `shouldBe` (Listy (Cons (Boolean True) (Cons (Boolean False) (Cons (Boolean True) Empty))))

arithmicTests = hspec $ do 
  describe "Evaluate numbering arithmics:" $ do 
    it "Addition" $ do 
      evaluateString "main = 1 + 2" `shouldBe` (Number 3 (Unit []))
      evaluateString "main = 1.1 + 2.5" `shouldBe` (Number 3.6 (Unit []))
    it "Subtraction" $ do 
      evaluateString "main = 2 - 1" `shouldBe` (Number 1 (Unit []))
      evaluateString "main = 2.5 - 0.5" `shouldBe` (Number 2.0 (Unit []))
    it "Multiplication" $ do 
      evaluateString "main = 2 * 3" `shouldBe` (Number 6 (Unit []))
      evaluateString "main = 2.0 * 3.5" `shouldBe` (Number 7.0 (Unit []))
    it "Divition" $ do
     evaluateString "main = 6 / 2" `shouldBe` (Number 3 (Unit []))
     evaluateString "main = 6.0 / 2.0" `shouldBe` (Number 3.0 (Unit []))
    it "Reminder" $ do 
      evaluateString "main = 5 % 2" `shouldBe` (Number 1 (Unit []))

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
      evaluateString "main = let x = 1 in x" `shouldBe` (Number 1 (Unit []))
      evaluateString "main = let y = True in y" `shouldBe` (Boolean True)
      evaluateString "main = let x = 1, y = 2 in y - x" `shouldBe` (Number 1 (Unit []))
    it "Let with arithmics" $ do 
      evaluateString "main = let x = 1 in x + 1" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = let x = True in x || False" `shouldBe` (Boolean True)
    it "Let expressions with multiple variables" $ do 
      evaluateString "main = let x = 1, y = 2 in x + y" `shouldBe` (Number 3 (Unit []))
      evaluateString "main = let x = True, y = False in y && x" `shouldBe` (Boolean False)
    it "Let expressions with functios" $ do 
      evaluateString "main = let f x = x in f 2" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = let not x = !x in not True" `shouldBe` (Boolean False)
    it "Let expressions with functions with multiple arguments" $ do
      evaluateString "main = let add x y = x + y in add 2 2" `shouldBe` (Number 4 (Unit []))
    it "If then else expressions:" $ do 
      evaluateString "main = if True then 1 else 0" `shouldBe` (Number 1 (Unit []))
      evaluateString "main = if False then 1 else 0" `shouldBe` (Number 0 (Unit []))

functionTests = hspec $ do 
  describe "Testing functions without parameters -" $ do 
    it "Call function from main" $ do 
      evaluateString "main = f; f = 1" `shouldBe` (Number 1 (Unit []))  
    it "Calling function from function" $ do 
      evaluateString "main = f; f = g; g = True" `shouldBe` (Boolean True)
  describe "Testing functions with one parameter -" $ do 
    it "Calling function with a number" $ do 
      evaluateString "main = f 1; f x = x" `shouldBe` (Number 1 (Unit []))
    it "Callning functions with booleans" $ do 
      evaluateString "main = f True; f x = x" `shouldBe` (Boolean True)
      evaluateString "main = not True; not x = !x" `shouldBe` (Boolean False)
    it "Calling functions with expressions" $ do 
      evaluateString "main = f 1 + 2; f x = x + 1" `shouldBe` (Number 4 (Unit [])) 
  describe "Testing functions with multiple parameters -" $ do 
    it "Calling functions with multiple numbers" $ do 
      evaluateString "main = add 1 2; add x y = x + y" `shouldBe` (Number 3 (Unit []))

evaluatingNumbersWithUnits = hspec $ do 
  describe "Numbers with base units -" $ do 
    it "Metre" $ evaluateString "main = 1 <<m>>" `shouldBe` (Number 1 (Unit [(Metre, None, 1)]))
    it "Second" $ evaluateString "main = 1 <<s>>" `shouldBe` (Number 1 (Unit [(Second, None, 1)]))
    it "Gram" $ evaluateString "main = 1 <<g>>" `shouldBe` (Number 1 (Unit [(Gram, None, 1)]))
    it "Ampere" $ evaluateString "main = 1 <<A>>" `shouldBe` (Number 1 (Unit [(Ampere, None, 1)]))
    it "Kelvin" $ evaluateString "main = 1 <<K>>" `shouldBe` (Number 1 (Unit [(Kelvin, None, 1)]))
    it "Mole" $ evaluateString "main = 1 <<mol>>" `shouldBe` (Number 1 (Unit [(Mole, None, 1)]))
    it "Candela" $ evaluateString "main = 1 <<cd>>" `shouldBe` (Number 1 (Unit [(Candela, None, 1)]))
  describe "Arithmic operations with units -" $ do 
    it "Addition" $ do 
      evaluateString "main = 1 <<m>> + 1 <<m>>" `shouldBe` (Number 2 (Unit [(Metre, None, 1)]))
      evaluateString "main = 1 <<s>> + 1 <<s>>" `shouldBe` (Number 2 (Unit [(Second, None, 1)]))
      evaluateString "main = 1 <<g>> + 1 <<g>>" `shouldBe` (Number 2 (Unit [(Gram, None, 1)]))
      evaluateString "main = 1 <<A>> + 1 <<A>>" `shouldBe` (Number 2 (Unit [(Ampere, None, 1)]))
      evaluateString "main = 1 <<K>> + 1 <<K>>" `shouldBe` (Number 2 (Unit [(Kelvin, None, 1)]))
      evaluateString "main = 1 <<mol>> + 1 <<mol>>" `shouldBe` (Number 2 (Unit [(Mole, None, 1)]))
      evaluateString "main = 1 <<cd>> + 1 <<cd>>" `shouldBe` (Number 2 (Unit [(Candela, None, 1)]))
    it "Subtraction" $ do 
      evaluateString "main = 1 <<m>> - 1 <<m>>" `shouldBe` (Number 0 (Unit [(Metre, None, 1)]))
      evaluateString "main = 1 <<s>> - 1 <<s>>" `shouldBe` (Number 0 (Unit [(Second, None, 1)]))
      evaluateString "main = 1 <<g>> - 1 <<g>>" `shouldBe` (Number 0 (Unit [(Gram, None, 1)]))
      evaluateString "main = 1 <<A>> - 1 <<A>>" `shouldBe` (Number 0 (Unit [(Ampere, None, 1)]))
      evaluateString "main = 1 <<K>> - 1 <<K>>" `shouldBe` (Number 0 (Unit [(Kelvin, None, 1)]))
      evaluateString "main = 1 <<mol>> - 1 <<mol>>" `shouldBe` (Number 0 (Unit [(Mole, None, 1)]))
      evaluateString "main = 1 <<cd>> - 1 <<cd>>" `shouldBe` (Number 0 (Unit [(Candela, None, 1)]))
    it "Multiplication" $ do 
      evaluateString "main = 4 <<m>> * 4 <<m>>" `shouldBe` (Number 16 (Unit [(Metre, None, 2)]))
      evaluateString "main = 4 <<s>> * 4 <<s>>" `shouldBe` (Number 16 (Unit [(Second, None, 2)]))
      evaluateString "main = 4 <<g>> * 4 <<g>>" `shouldBe` (Number 16 (Unit [(Gram, None, 2)]))
      evaluateString "main = 4 <<A>> * 4 <<A>>" `shouldBe` (Number 16 (Unit [(Ampere, None, 2)]))
      evaluateString "main = 4 <<K>> * 4 <<K>>" `shouldBe` (Number 16 (Unit [(Kelvin, None, 2)]))
      evaluateString "main = 4 <<mol>> * 4 <<mol>>" `shouldBe` (Number 16 (Unit [(Mole, None, 2)]))
      evaluateString "main = 4 <<cd>> * 4 <<cd>>" `shouldBe` (Number 16 (Unit [(Candela, None, 2)]))
    it "Division" $ do 
      evaluateString "main = 4 <<m>> / 2 <<m>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 4 <<s>> / 2 <<s>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 4 <<g>> / 2 <<g>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 4 <<A>> / 2 <<A>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 4 <<K>> / 2 <<K>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 4 <<mol>> / 2 <<mol>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 4 <<cd>> / 2 <<cd>>" `shouldBe` (Number 2 (Unit []))
  describe "Arithmics with prefixed units -" $ do 
    it "Addition" $ do
      evaluateString "main = 1 <<km>> + 400 <<m>>" `shouldBe` (Number 1400 (Unit [(Metre, None, 1)]))
      evaluateString "main = 1 <<dam>> + 200 <<mm>>" `shouldBe` (Number 10200 (Unit [(Metre, Milli, 1)]))
      evaluateString "main = 10 <<A^2>> + 20 <<mA^2>>" `shouldBe` (Number 10020 (Unit [(Ampere, Milli, 2)]))
      evaluateString "main = 1 <<>> + 2 <<>>" `shouldBe` (Number 3 (Unit []))
    it "Subtraction" $ do 
      evaluateString "main = 10 <<km>> - 100 <<dam>>" `shouldBe` (Number 900 (Unit [(Metre, Deca, 1)]))
    it "Multiplication" $ do 
      evaluateString "main = 10 <<km>> * 10 <<km>>" `shouldBe` (Number 100 (Unit [(Metre, Kilo, 2)]))
      evaluateString "main = 10 <<km>> * 10 <<m>>" `shouldBe` (Number 100000 (Unit [(Metre, None, 2)]))
      evaluateString "main = 10 <<m>> * 10 <<m^-1>>" `shouldBe` (Number 100 (Unit []))
      evaluateString "main = 10 <<m^3>> * 10 <<m^5>>" `shouldBe` (Number 100 (Unit [(Metre, None, 8)]))
    it "Division" $ do 
      evaluateString "main = 10 <<m>> / 5 <<m>>" `shouldBe` (Number 2 (Unit []))
      evaluateString "main = 10 <<m^2>> / 2 <<m>>" `shouldBe` (Number 5 (Unit [(Metre, None, 1)]))
  describe "Arithmics with multiple units -" $ do 
    it "Metres and seconds" $ do 
      evaluateString "main = 10 <<m * s>> + 20 <<m * s>>" `shouldBe` (Number 30 (Unit [(Metre, None, 1), (Second, None, 1)]))