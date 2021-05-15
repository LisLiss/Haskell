module Test11 (test11) where

import Block1.Task11
import Test.Tasty
import Test.Tasty.HUnit

testStringSum :: Assertion
testStringSum = do
  stringSum "2 -3 4 21 8" @?= Just 32
  stringSum "a" @?= Nothing
  stringSum "2" @?= Just 2
  stringSum "-5" @?= Just (-5)
  stringSum "2   8  -2" @?= Just 8
  stringSum "2 + 3" @?= Nothing
  stringSum "" @?= Just 0

test11 :: TestTree
test11 =
  testGroup
  "Testing Block1 Task1 (stringSum)"
  [ testCase "testing stringSum" testStringSum ]



