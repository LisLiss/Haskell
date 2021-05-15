module Test32 (test32) where

import Block3.Task31
import Block3.Task32
import Block3.Task33
import Block3.Task34
import Test.Tasty
import Test.Tasty.HUnit
import Data.Char (isDigit, isSpace)
import Control.Monad
import Control.Applicative

testOk :: Assertion
testOk = do
  runParser ok "123" @?= Just ((), "123")
  runParser ok "qwerty12lkmde" @?= Just ((), "qwerty12lkmde")
  runParser ok "" @?= Just ((), "")
  runParser ok [5] @?= Just ((), [5])
  runParser ok [-7, 34, 22] @?= Just ((), [-7, 34, 22])
  runParser ok ["kmd", "lol", "kek"] @?= Just ((), ["kmd", "lol", "kek"])

testEof :: Assertion
testEof = do
  runParser eof "123" @?= Nothing
  runParser eof "qwerty12lkmde" @?= Nothing
  runParser eof "" @?= Just ((), "")
  runParser eof ["kmd", "lol", "kek"] @?= Nothing
  runParser eof [""] @?= Nothing
  runParser eof [5] @?= Nothing

testSatisfy :: Assertion
testSatisfy = do
  runParser (satisfy isDigit) "12345" @?= Just ('1', "2345")
  runParser (satisfy isDigit) "k345" @?= Nothing
  runParser (satisfy isSpace) "12345" @?= Nothing
  runParser (satisfy isSpace) " k12345" @?= Just (' ', "k12345")
  runParser (satisfy (==1)) [1, 2, 0, 4] @?= Just (1, [2, 0, 4])
  runParser (satisfy (==2) <|> satisfy (==1)) [1, 2, 0, 4] @?= Just (1, [2, 0, 4])
  runParser (satisfy (==2) <|> satisfy (==3)) [1, 2, 0, 4] @?= Nothing

testElement :: Assertion
testElement = do
  runParser (element '1') "12345" @?= Just ('1', "2345")
  runParser (element '1') "1" @?= Just ('1', "")
  runParser (element 'k') "k12345" @?= Just ('k', "12345")
  runParser (element '1') "qweerty" @?= Nothing
  runParser (element '1') "" @?= Nothing
  runParser (element ' ') " 12345" @?= Just (' ', "12345")
  runParser (element '.') ".2345" @?= Just ('.', "2345")
  runParser (element 2 <|> element 1) [1, 2, 0, 4] @?= Just (1, [2, 0, 4])
  runParser (element 2 <|> element 3) [1, 2, 0, 4] @?= Nothing

testStream :: Assertion
testStream = do
  runParser (stream "1") "12345" @?= Just ("1", "2345")
  runParser (stream "123") "12345" @?= Just ("123", "45")
  runParser (stream "1") "k12345" @?= Nothing
  runParser (stream "kek") "kek" @?= Just ("kek", "")
  runParser (stream "kek") "ke" @?= Nothing
  runParser (stream "kek") "keklol" @?= Just ("kek", "lol")

test32 :: TestTree
test32 =
  testGroup
  "Testing Block3 Task2 (Parser)"
  [ testCase "testing OK" testOk
  , testCase "testing EOF" testEof
  , testCase "testing satisfy" testSatisfy
  , testCase "testing element" testElement
  , testCase "testing stream" testStream ]



