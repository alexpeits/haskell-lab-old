module SchemeParser.ParserSpec where

import Test.Hspec

import Data.Ratio ((%))
import Data.Complex (Complex(..))

import qualified Data.Map as M
import Text.Parsec (runParser)

import SchemeParser.Types
import SchemeParser.Parser
import SchemeParser.Printer

main :: IO ()
main = hspec spec

parse :: String -> Either () LispVal
parse input = case runParser parseExpr M.empty "scheme-test" input of
  Left err -> Left ()
  Right val -> Right val

spec :: Spec
spec =
  describe "parser" $ do
    it "parses atoms" $ do
      parse "atom" `shouldBe` Right (LAtom "atom")
      parse "atom123" `shouldBe` Right (LAtom "atom123")
      parse "atom!$%" `shouldBe` Right (LAtom "atom!$%")
      parse "!atom" `shouldBe` Right (LAtom "!atom")
      parse "!123" `shouldBe` Right (LAtom "!123")
      parse "!atom123" `shouldBe` Right (LAtom "!atom123")
      parse "!123atom" `shouldBe` Right (LAtom "!123atom")
      parse "!%123$atom%$" `shouldBe` Right (LAtom "!%123$atom%$")
    it "parses strings" $ do
      parse "\"string\"" `shouldBe` Right (LString "string")
      parse "\"string123\"" `shouldBe` Right (LString "string123")
      parse "\"string!$%\"" `shouldBe` Right (LString "string!$%")
      parse "\"!string\"" `shouldBe` Right (LString "!string")
      parse "\"!123\"" `shouldBe` Right (LString "!123")
      parse "\"!string123\"" `shouldBe` Right (LString "!string123")
      parse "\"!123string\"" `shouldBe` Right (LString "!123string")
      parse "\"!%123$string%$\"" `shouldBe` Right (LString "!%123$string%$")
      parse "\"string\\\"nested\\\"123\"" `shouldBe` Right (LString "string\"nested\"123")
      parse "\"string'nested singlequote'123\"" `shouldBe` Right (LString "string'nested singlequote'123")
    it "parses bools" $ do
      parse "#t" `shouldBe` Right (LBool True)
      parse "#f" `shouldBe` Right (LBool False)
    it "parses chars" $ do
      parse "#\\a" `shouldBe` Right (LChar 'a')
      parse "#\\ " `shouldBe` Right (LChar ' ')
      parse "#\\space" `shouldBe` Right (LChar ' ')
      parse "#\\newline" `shouldBe` Right (LChar '\n')
      parse "#\\n" `shouldBe` Right (LChar 'n')
    it "parses numbers" $ do
      parse "123" `shouldBe` Right (LNumber 123)
      parse "#d123" `shouldBe` Right (LNumber 123)
      parse "#b1010" `shouldBe` Right (LNumber 10)
      parse "#o123" `shouldBe` Right (LNumber 83)
      parse "#xff" `shouldBe` Right (LNumber 255)
    it "parses floats" $ do
      parse "123.123" `shouldBe` Right (LFloat 123.123)
      parse "0.123" `shouldBe` Right (LFloat 0.123)
      parse "123.0" `shouldBe` Right (LFloat 123.0)
    it "parses ratios" $ do
      parse "10/3" `shouldBe` Right (LRatio (10 % 3))
      parse "25/5" `shouldBe` Right (LRatio (25 % 5))
      parse "25/5" `shouldBe` Right (LRatio (5 % 1))
    it "parses complex numbers" $ do
      parse "1+1i" `shouldBe` Right (LComplex (1 :+ 1))
      parse "123+456i" `shouldBe` Right (LComplex (123 :+ 456))
      -- TODO: test floats
    it "parses lists" $ do
      parse "()" `shouldBe` Right (LList [])
      parse "(atom)" `shouldBe` Right (LList [LAtom "atom"])
      parse "(one two three)" `shouldBe` Right (LList [LAtom "one", LAtom "two", LAtom "three"])
      parse "(\"foo\")" `shouldBe` Right (LList [LString "foo"])
      parse "(\"one\" \"two\" \"three\")" `shouldBe` Right (LList [LString "one", LString "two", LString "three"])
      parse "(#\\a)" `shouldBe` Right (LList [LChar 'a'])
      parse "(#\\a #\\  #\\space #\\newline #\\n)" `shouldBe` Right (LList [LChar 'a', LChar ' ', LChar ' ', LChar '\n', LChar 'n'])
      parse "(#t)" `shouldBe` Right (LList [LBool True])
      parse "(1)" `shouldBe` Right (LList [LNumber 1])
      parse "(1 2 3)" `shouldBe` Right (LList [LNumber 1, LNumber 2, LNumber 3])
      parse "(#d1)" `shouldBe` Right (LList [LNumber 1])
      parse "(#d1 #d2 #d3)" `shouldBe` Right (LList [LNumber 1, LNumber 2, LNumber 3])
      parse "(#o1)" `shouldBe` Right (LList [LNumber 1])
      parse "(#o1 #o2 #o3)" `shouldBe` Right (LList [LNumber 1, LNumber 2, LNumber 3])
      parse "(#b1)" `shouldBe` Right (LList [LNumber 1])
      parse "(#b1 #b10 #b11)" `shouldBe` Right (LList [LNumber 1, LNumber 2, LNumber 3])
      parse "(#x1)" `shouldBe` Right (LList [LNumber 1])
      parse "(#x1 #x2 #x3)" `shouldBe` Right (LList [LNumber 1, LNumber 2, LNumber 3])
      parse "(12.34)" `shouldBe` Right (LList [LFloat 12.34])
      parse "(12.34 56.78 90.12)" `shouldBe` Right (LList [LFloat 12.34, LFloat 56.78, LFloat 90.12])
      parse "(12/34)" `shouldBe` Right (LList [LRatio (12 % 34)])
      parse "(12/34 56/78 90/12)" `shouldBe` Right (LList [LRatio (12 % 34), LRatio (56 % 78), LRatio (90 % 12)])
      parse "(12+34i)" `shouldBe` Right (LList [LComplex (12 :+ 34)])
      parse "(12+34i 56+78i 90+12i)" `shouldBe` Right (LList [LComplex (12 :+ 34), LComplex (56 :+ 78), LComplex (90 :+ 12)])
      -- TODO: test complex with floats
      -- TODO: test more stuff
