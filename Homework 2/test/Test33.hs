module Test33 (test33) where

import Block3.Task31
import Block3.Task32
import Block3.Task33
import Block3.Task34
import Test.Tasty
import Test.Tasty.HUnit
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust, isNothing)

showFromMaybe :: Show x => Maybe (x, String) -> String
showFromMaybe Nothing = "Nothing"
showFromMaybe input = fst (fromJust (fmap (forFirst show) input))
  where
    forFirst f (x, y) = (f x, y)

testBalancedSequences :: Assertion
testBalancedSequences = do
  showFromMaybe (runParser balancedSequencesParser "()") @?= "()"
  showFromMaybe (runParser balancedSequencesParser "()()") @?= "()()"
  showFromMaybe (runParser balancedSequencesParser "(())") @?= "(())"
  showFromMaybe (runParser balancedSequencesParser "(())()") @?= "(())()"
  showFromMaybe (runParser balancedSequencesParser "") @?= ""

testUnBalancedSequences :: Assertion
testUnBalancedSequences = do
  showFromMaybe (runParser balancedSequencesParser ")") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "(()") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "(((((()))))") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser ")()") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "))))()((((") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "a") @?= "Nothing"

testIncorrectSequences :: Assertion
testIncorrectSequences = do
  showFromMaybe (runParser balancedSequencesParser "aaa") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "(a)") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser ")a(") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "((((()))))a") @?= "Nothing"
  showFromMaybe (runParser balancedSequencesParser "(a(((()))))") @?= "Nothing"

testIntParser :: Assertion
testIntParser = do
  showFromMaybe (runParser intParser "aaa") @?= "Nothing"
  showFromMaybe (runParser intParser "aaa111") @?= "Nothing"
  showFromMaybe (runParser intParser "1") @?= "1"
  showFromMaybe (runParser intParser "-123") @?= "-123"
  showFromMaybe (runParser intParser "+123") @?= "123"
  showFromMaybe (runParser intParser "111111") @?= "111111"
  showFromMaybe (runParser intParser "111111 aaa") @?= "111111"
  showFromMaybe (runParser intParser "1 aa 11111") @?= "1"
  showFromMaybe (runParser intParser "a 111111") @?= "Nothing"


test33 :: TestTree
test33 =
  testGroup
  "Testing Block3 Task3 (Simple Parser)"
  [ testCase "testing balanced sequences of brackets" testBalancedSequences
  , testCase "testing unbalanced sequences of brackets"  testUnBalancedSequences
  , testCase "testing unbalanced sequences of brackets"  testIncorrectSequences
  , testCase "testing intParser"  testIntParser ]



