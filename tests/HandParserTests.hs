module Main where

import HandSyntax
import HandLexer 
import HandParser 
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do 
    testArithmics

-- Testing all the arithmic operations
testArithmics = 
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