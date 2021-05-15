module Test12 (test12) where

import Block1.Task12
import Test.Tasty
import Test.Tasty.HUnit

testFunctorLow :: Assertion
testFunctorLow = do
  fmap id Leaf 1 @?= Leaf 1
  fmap id Branch (Leaf 1) (Leaf 2) @?= Branch (Leaf 1) (Leaf 2)
  fmap id Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2) @?= Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)
  -- fmap (f . g) = fmap f . fmap g
  fmap ((*10) . (+5)) (Leaf 1) @?= fmap (*10) (fmap (+5) (Leaf 1))
  fmap ((+2) . (*9)) (Branch (Leaf 1) (Leaf 2)) @?= fmap (+2) (fmap (*9) (Branch (Leaf 1) (Leaf 2)))

testApplicativeLow :: Assertion
testApplicativeLow = do
  -- pure id <*> x = x
  (pure id <*> (Leaf 1)) @?= (Leaf 1)
  (pure id <*> (Branch (Leaf 1) (Leaf 2))) @?= (Branch (Leaf 1) (Leaf 2))
  --composition
  let w = Leaf 5
  let u = fmap (+) w
  let v = fmap (*) w
  (pure (.) <*> u <*> v <*> w) @?= (u <*> (v <*> w))
  --interchange
  (u <*> pure 5) @?= (pure ($ 5) <*> u)

testFoldable :: Assertion
testFoldable = do
  foldr (+) 1 (Branch (Leaf 1) (Leaf 2)) @?= 4
  foldr (*) 5 (Branch (Leaf 7) (Leaf 2)) @?= 70
  foldr (-) 5 (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 2)) @?= -4


test12 :: TestTree
test12 =
  testGroup
  "Testing Block1 Task2 (Tree)"
  [ testCase "testing functor low" testFunctorLow
   , testCase "testing appplicative low" testApplicativeLow
   , testCase "testing foldr" testFoldable ]



