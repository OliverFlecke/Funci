module Main where

import Syntax
import Lexer
import Parser
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Applying default units to numbers -" $ do
    it "1 <<m>>" $ parseExpressionsString "1 <<m>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Metre, 1)])))
    it "1 <<s>>" $ parseExpressionsString "1 <<s>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Second, 1)])))
    it "1 <<g>>" $ parseExpressionsString "1 <<g>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Gram, 1)])))
    it "1 <<A>>" $ parseExpressionsString "1 <<A>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Ampere, 1)])))
    it "1 <<K>>" $ parseExpressionsString "1 <<K>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Kelvin, 1)])))
    it "1 <<mol>>" $ parseExpressionsString "1 <<mol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Mole, 1)])))
    it "1 <<cd>>" $ parseExpressionsString "1 <<cd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Candela, 1)])))
  describe "Apply units with prefixes for metre -" $ do
    it "1 <<Ym>>" $ parseExpressionsString "1 <<Ym>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Metre, 1)])))
    it "1 <<Zm>>" $ parseExpressionsString "1 <<Zm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Metre, 1)])))
    it "1 <<Em>>" $ parseExpressionsString "1 <<Em>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Metre, 1)])))
    it "1 <<Pm>>" $ parseExpressionsString "1 <<Pm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Metre, 1)])))
    it "1 <<Tm>>" $ parseExpressionsString "1 <<Tm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Metre, 1)])))
    it "1 <<Gm>>" $ parseExpressionsString "1 <<Gm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Metre, 1)])))
    it "1 <<Mm>>" $ parseExpressionsString "1 <<Mm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Metre, 1)])))
    it "1 <<km>>" $ parseExpressionsString "1 <<km>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Metre, 1)])))
    it "1 <<hm>>" $ parseExpressionsString "1 <<hm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Metre, 1)])))
    it "1 <<dam>>" $ parseExpressionsString "1 <<dam>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Metre, 1)])))
    it "1 <<dm>>" $ parseExpressionsString "1 <<dm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Metre, 1)])))
    it "1 <<cm>>" $ parseExpressionsString "1 <<cm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Metre, 1)])))
    it "1 <<mm>>" $ parseExpressionsString "1 <<mm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Metre, 1)])))
    it "1 <<mum>>" $ parseExpressionsString "1 <<mum>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Metre, 1)])))
    it "1 <<nm>>" $ parseExpressionsString "1 <<nm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Metre, 1)])))
    it "1 <<pm>>" $ parseExpressionsString "1 <<pm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Metre, 1)])))
    it "1 <<fm>>" $ parseExpressionsString "1 <<fm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Metre, 1)])))
    it "1 <<am>>" $ parseExpressionsString "1 <<am>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Metre, 1)])))
    it "1 <<zm>>" $ parseExpressionsString "1 <<zm>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Metre, 1)])))
    it "1 <<ym>>" $ parseExpressionsString "1 <<ym>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Metre, 1)])))
  describe "Find prefixes for seconds -" $ do
    it "1 <<Ys>>" $ parseExpressionsString "1 <<Ys>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Second, 1)])))
    it "1 <<Zs>>" $ parseExpressionsString "1 <<Zs>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Second, 1)])))
    it "1 <<Es>>" $ parseExpressionsString "1 <<Es>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Second, 1)])))
    it "1 <<Ps>>" $ parseExpressionsString "1 <<Ps>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Second, 1)])))
    it "1 <<Ts>>" $ parseExpressionsString "1 <<Ts>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Second, 1)])))
    it "1 <<Gs>>" $ parseExpressionsString "1 <<Gs>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Second, 1)])))
    it "1 <<Ms>>" $ parseExpressionsString "1 <<Ms>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Second, 1)])))
    it "1 <<ks>>" $ parseExpressionsString "1 <<ks>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Second, 1)])))
    it "1 <<hs>>" $ parseExpressionsString "1 <<hs>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Second, 1)])))
    it "1 <<das>>" $ parseExpressionsString "1 <<das>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Second, 1)])))
    it "1 <<ds>>" $ parseExpressionsString "1 <<ds>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Second, 1)])))
    it "1 <<cs>>" $ parseExpressionsString "1 <<cs>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Second, 1)])))
    it "1 <<ms>>" $ parseExpressionsString "1 <<ms>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Second, 1)])))
    it "1 <<mus>>" $ parseExpressionsString "1 <<mus>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Second, 1)])))
    it "1 <<ns>>" $ parseExpressionsString "1 <<ns>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Second, 1)])))
    it "1 <<ps>>" $ parseExpressionsString "1 <<ps>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Second, 1)])))
    it "1 <<fs>>" $ parseExpressionsString "1 <<fs>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Second, 1)])))
    it "1 <<as>>" $ parseExpressionsString "1 <<as>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Second, 1)])))
    it "1 <<zs>>" $ parseExpressionsString "1 <<zs>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Second, 1)])))
    it "1 <<ys>>" $ parseExpressionsString "1 <<ys>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Second, 1)])))
  describe "Find prefixes for gram -" $ do
    it "1 <<Yg>>" $ parseExpressionsString "1 <<Yg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Gram, 1)])))
    it "1 <<Zg>>" $ parseExpressionsString "1 <<Zg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Gram, 1)])))
    it "1 <<Eg>>" $ parseExpressionsString "1 <<Eg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Gram, 1)])))
    it "1 <<Pg>>" $ parseExpressionsString "1 <<Pg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Gram, 1)])))
    it "1 <<Tg>>" $ parseExpressionsString "1 <<Tg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Gram, 1)])))
    it "1 <<Gg>>" $ parseExpressionsString "1 <<Gg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Gram, 1)])))
    it "1 <<Mg>>" $ parseExpressionsString "1 <<Mg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Gram, 1)])))
    it "1 <<kg>>" $ parseExpressionsString "1 <<kg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Gram, 1)])))
    it "1 <<hg>>" $ parseExpressionsString "1 <<hg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Gram, 1)])))
    it "1 <<dag>>" $ parseExpressionsString "1 <<dag>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Gram, 1)])))
    it "1 <<dg>>" $ parseExpressionsString "1 <<dg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Gram, 1)])))
    it "1 <<cg>>" $ parseExpressionsString "1 <<cg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Gram, 1)])))
    it "1 <<mg>>" $ parseExpressionsString "1 <<mg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Gram, 1)])))
    it "1 <<mug>>" $ parseExpressionsString "1 <<mug>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Gram, 1)])))
    it "1 <<ng>>" $ parseExpressionsString "1 <<ng>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Gram, 1)])))
    it "1 <<pg>>" $ parseExpressionsString "1 <<pg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Gram, 1)])))
    it "1 <<fg>>" $ parseExpressionsString "1 <<fg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Gram, 1)])))
    it "1 <<ag>>" $ parseExpressionsString "1 <<ag>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Gram, 1)])))
    it "1 <<zg>>" $ parseExpressionsString "1 <<zg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Gram, 1)])))
    it "1 <<yg>>" $ parseExpressionsString "1 <<yg>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Gram, 1)])))
  describe "Find prefixes for Ampere -" $ do
    it "1 <<YA>>" $ parseExpressionsString "1 <<YA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Ampere, 1)])))
    it "1 <<ZA>>" $ parseExpressionsString "1 <<ZA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Ampere, 1)])))
    it "1 <<EA>>" $ parseExpressionsString "1 <<EA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Ampere, 1)])))
    it "1 <<PA>>" $ parseExpressionsString "1 <<PA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Ampere, 1)])))
    it "1 <<TA>>" $ parseExpressionsString "1 <<TA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Ampere, 1)])))
    it "1 <<GA>>" $ parseExpressionsString "1 <<GA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Ampere, 1)])))
    it "1 <<MA>>" $ parseExpressionsString "1 <<MA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Ampere, 1)])))
    it "1 <<kA>>" $ parseExpressionsString "1 <<kA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Ampere, 1)])))
    it "1 <<hA>>" $ parseExpressionsString "1 <<hA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Ampere, 1)])))
    it "1 <<daA>>" $ parseExpressionsString "1 <<daA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Ampere, 1)])))
    it "1 <<dA>>" $ parseExpressionsString "1 <<dA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Ampere, 1)])))
    it "1 <<cA>>" $ parseExpressionsString "1 <<cA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Ampere, 1)])))
    it "1 <<mA>>" $ parseExpressionsString "1 <<mA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Ampere, 1)])))
    it "1 <<muA>>" $ parseExpressionsString "1 <<muA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Ampere, 1)])))
    it "1 <<nA>>" $ parseExpressionsString "1 <<nA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Ampere, 1)])))
    it "1 <<pA>>" $ parseExpressionsString "1 <<pA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Ampere, 1)])))
    it "1 <<fA>>" $ parseExpressionsString "1 <<fA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Ampere, 1)])))
    it "1 <<aA>>" $ parseExpressionsString "1 <<aA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Ampere, 1)])))
    it "1 <<zA>>" $ parseExpressionsString "1 <<zA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Ampere, 1)])))
    it "1 <<yA>>" $ parseExpressionsString "1 <<yA>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Ampere, 1)])))
  describe "Find prefixes for Kelvin -" $ do
    it "1 <<YK>>" $ parseExpressionsString "1 <<YK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Kelvin, 1)])))
    it "1 <<ZK>>" $ parseExpressionsString "1 <<ZK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Kelvin, 1)])))
    it "1 <<EK>>" $ parseExpressionsString "1 <<EK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Kelvin, 1)])))
    it "1 <<PK>>" $ parseExpressionsString "1 <<PK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Kelvin, 1)])))
    it "1 <<TK>>" $ parseExpressionsString "1 <<TK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Kelvin, 1)])))
    it "1 <<GK>>" $ parseExpressionsString "1 <<GK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Kelvin, 1)])))
    it "1 <<MK>>" $ parseExpressionsString "1 <<MK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Kelvin, 1)])))
    it "1 <<kK>>" $ parseExpressionsString "1 <<kK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Kelvin, 1)])))
    it "1 <<hK>>" $ parseExpressionsString "1 <<hK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Kelvin, 1)])))
    it "1 <<daK>>" $ parseExpressionsString "1 <<daK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Kelvin, 1)])))
    it "1 <<dK>>" $ parseExpressionsString "1 <<dK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Kelvin, 1)])))
    it "1 <<cK>>" $ parseExpressionsString "1 <<cK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Kelvin, 1)])))
    it "1 <<mK>>" $ parseExpressionsString "1 <<mK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Kelvin, 1)])))
    it "1 <<muK>>" $ parseExpressionsString "1 <<muK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Kelvin, 1)])))
    it "1 <<nK>>" $ parseExpressionsString "1 <<nK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Kelvin, 1)])))
    it "1 <<pK>>" $ parseExpressionsString "1 <<pK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Kelvin, 1)])))
    it "1 <<fK>>" $ parseExpressionsString "1 <<fK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Kelvin, 1)])))
    it "1 <<aK>>" $ parseExpressionsString "1 <<aK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Kelvin, 1)])))
    it "1 <<zK>>" $ parseExpressionsString "1 <<zK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Kelvin, 1)])))
    it "1 <<yK>>" $ parseExpressionsString "1 <<yK>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Kelvin, 1)])))
  describe "Find prefixes for Mole -" $ do
    it "1 <<Ymol>>" $ parseExpressionsString "1 <<Ymol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Mole, 1)])))
    it "1 <<Zmol>>" $ parseExpressionsString "1 <<Zmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Mole, 1)])))
    it "1 <<Emol>>" $ parseExpressionsString "1 <<Emol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Mole, 1)])))
    it "1 <<Pmol>>" $ parseExpressionsString "1 <<Pmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Mole, 1)])))
    it "1 <<Tmol>>" $ parseExpressionsString "1 <<Tmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Mole, 1)])))
    it "1 <<Gmol>>" $ parseExpressionsString "1 <<Gmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Mole, 1)])))
    it "1 <<Mmol>>" $ parseExpressionsString "1 <<Mmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Mole, 1)])))
    it "1 <<kmol>>" $ parseExpressionsString "1 <<kmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Mole, 1)])))
    it "1 <<hmol>>" $ parseExpressionsString "1 <<hmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Mole, 1)])))
    it "1 <<damol>>" $ parseExpressionsString "1 <<damol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Mole, 1)])))
    it "1 <<dmol>>" $ parseExpressionsString "1 <<dmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Mole, 1)])))
    it "1 <<cmol>>" $ parseExpressionsString "1 <<cmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Mole, 1)])))
    it "1 <<mmol>>" $ parseExpressionsString "1 <<mmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Mole, 1)])))
    it "1 <<mumol>>" $ parseExpressionsString "1 <<mumol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Mole, 1)])))
    it "1 <<nmol>>" $ parseExpressionsString "1 <<nmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Mole, 1)])))
    it "1 <<pmol>>" $ parseExpressionsString "1 <<pmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Mole, 1)])))
    it "1 <<fmol>>" $ parseExpressionsString "1 <<fmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Mole, 1)])))
    it "1 <<amol>>" $ parseExpressionsString "1 <<amol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Mole, 1)])))
    it "1 <<zmol>>" $ parseExpressionsString "1 <<zmol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Mole, 1)])))
    it "1 <<ymol>>" $ parseExpressionsString "1 <<ymol>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Mole, 1)])))
  describe "Find prefixes for Candela -" $ do
    it "1 <<Ycd>>" $ parseExpressionsString "1 <<Ycd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yotta, Candela, 1)])))
    it "1 <<Zcd>>" $ parseExpressionsString "1 <<Zcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zetta, Candela, 1)])))
    it "1 <<Ecd>>" $ parseExpressionsString "1 <<Ecd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Exa, Candela, 1)])))
    it "1 <<Pcd>>" $ parseExpressionsString "1 <<Pcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Peta, Candela, 1)])))
    it "1 <<Tcd>>" $ parseExpressionsString "1 <<Tcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Tera, Candela, 1)])))
    it "1 <<Gcd>>" $ parseExpressionsString "1 <<Gcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Giga, Candela, 1)])))
    it "1 <<Mcd>>" $ parseExpressionsString "1 <<Mcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Mega, Candela, 1)])))
    it "1 <<kcd>>" $ parseExpressionsString "1 <<kcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Candela, 1)])))
    it "1 <<hcd>>" $ parseExpressionsString "1 <<hcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Hecto, Candela, 1)])))
    it "1 <<dacd>>" $ parseExpressionsString "1 <<dacd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deca, Candela, 1)])))
    it "1 <<dcd>>" $ parseExpressionsString "1 <<dcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Deci, Candela, 1)])))
    it "1 <<ccd>>" $ parseExpressionsString "1 <<ccd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Centi, Candela, 1)])))
    it "1 <<mcd>>" $ parseExpressionsString "1 <<mcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Milli, Candela, 1)])))
    it "1 <<mucd>>" $ parseExpressionsString "1 <<mucd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Micro, Candela, 1)])))
    it "1 <<ncd>>" $ parseExpressionsString "1 <<ncd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Nano, Candela, 1)])))
    it "1 <<pcd>>" $ parseExpressionsString "1 <<pcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Pico, Candela, 1)])))
    it "1 <<fcd>>" $ parseExpressionsString "1 <<fcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Femto, Candela, 1)])))
    it "1 <<acd>>" $ parseExpressionsString "1 <<acd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Atto, Candela, 1)])))
    it "1 <<zcd>>" $ parseExpressionsString "1 <<zcd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Zepto, Candela, 1)])))
    it "1 <<ycd>>" $ parseExpressionsString "1 <<ycd>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Yocto, Candela, 1)])))
  describe "Units with exponents -" $ do
    it "1 <<m^2>>" $ parseExpressionsString "1 <<m^2>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Metre, 2)])))
    it "1 <<m^-1>>" $ parseExpressionsString "1 <<m^-1>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Metre, -1)])))
  describe "Multiple units -" $ do
    it "1 <<m*s>>" $ parseExpressionsString "1 <<m*s>>" `shouldBe` Right (Const (Number (I 1) (Unit [(None, Metre, 1), (None, Second, 1)])))
    it "1 <<km^2*s^-1>>" $ parseExpressionsString "1 <<km^2 * s^-1>>" `shouldBe` Right (Const (Number (I 1) (Unit [(Kilo, Metre, 2), (None, Second, -1)])))