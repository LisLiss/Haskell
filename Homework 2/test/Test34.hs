module Test34 (test34) where

import Block3.Task31
import Block3.Task32
import Block3.Task33
import Block3.Task34
import Test.Tasty
import Test.Tasty.HUnit
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust, isNothing)

testListParser :: Assertion
testListParser = do
  runParser listParser "2,    10, -5" @?= Just ([10, -5], "")
  runParser listParser "4, 2, 7  , 10, -5" @?= Just ([2, 7, 10, -5], "")
  runParser listParser "0" @?= Just ([], "")
  runParser listParser "2, +7, -2" @?= Just ([7, -2], "")
  runParser listParser "2, aaa, bbb" @?= Nothing
  runParser listParser "2, +7, +-2" @?= Nothing
  runParser listParser "-2, 1, 1" @?= Nothing
  
testListlistParser :: Assertion
testListlistParser = do
  runParser listlistParser "2,    10, -5, 3, +2, 0, -3" @?= Just ([[10, -5], [2, 0, -3]], "")
  runParser listlistParser "4, 2, 7  , 10, -5" @?= Just ([[2, 7, 10, -5]], "")
  runParser listlistParser "0" @?= Just ([[]], "")
  runParser listlistParser "+2, +7, -2" @?= Just ([[7, -2]], "")
  runParser listlistParser "2, aaa, bbb" @?= Nothing
  runParser listlistParser "2, +7, +-2" @?= Nothing
  runParser listlistParser "2, +7, +-2, 1, 0" @?= Nothing
  runParser listlistParser "-4, +7, -2, 1, 0" @?= Nothing


test34 :: TestTree
test34 =
  testGroup
  "Testing Block3 Task4 (Hard Parser)"
  [ testCase "testing list Parser" testListParser
  , testCase "testing listlist Parser"  testListlistParser
  ]


