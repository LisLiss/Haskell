module Test13 (test13) where

import Block1.Task13
import Test.Tasty
import Test.Tasty.HUnit

testFunctorLow :: Assertion
testFunctorLow = do
  fmap id ((1 :: Int) :| []) @?= ( 1 :| [])
  fmap id ((1 :: Int) :| [2, 6]) @?= ( 1 :| [2, 6])
  fmap id (('s' :: Char) :| ['p', 'r']) @?= ( 's' :| ['p', 'r'])
  --fmap (f . g) = fmap f . fmap g
  fmap ((*10) . (+5)) ((1 :: Int) :| []) @?= fmap (*10) (fmap (+5) ( (1 :: Int) :| []))
  
testApplicativeLow :: Assertion
testApplicativeLow = do
  -- pure id <*> x = x
  (pure id <*> ((1 :: Int) :| [2, 6])) @?= ((1 :: Int) :| [2, 6])
  --composition
  let w = ( (1 :: Int) :| [])
  let u = fmap (+) w
  let v = fmap (*) w
  (pure (.) <*> u <*> v <*> w) @?= (u <*> (v <*> w))
  --interchange
  (u <*> pure 5) @?= (pure ($ 5) <*> u)

testFoldable :: Assertion
testFoldable = do
  foldr (+) 1 ((1 :: Int) :| []) @?= 2
  foldr (*) 5 ((1 :: Int) :| [2, 6]) @?= 60
  foldr (-) 5 ((-10 :: Int) :| [12, 32, -5, 9, 6]) @?= 23


test13 :: TestTree
test13 =
  testGroup
  "Testing Block1 Task3 (NonEmpty)"
  [ testCase "testing functor low" testFunctorLow
   , testCase "testing applicative low" testApplicativeLow
   , testCase "testing foldr" testFoldable ]



