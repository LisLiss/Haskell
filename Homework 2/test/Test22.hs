module Test22 (test22) where

import Block2.Task22
import Test.Tasty
import Test.Tasty.HUnit

testMoving :: Assertion
testMoving = do
  moving 4 [1, 5, 3, 8, 7, 9, 6] @?= [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
  moving 2 [1, 5, 3, 8, 7, 9, 6] @?= [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
  moving 2 [9, -2, 3, 4, 1, -5, 0] @?= [9.0,3.5,0.5,3.5,2.5,-2.0,-2.5]
  moving 1 [100] @?= [100.0]
  moving 10 [100] @?= [100.0]
  moving 3 [1, 1, 1] @?= [1.0, 1.0, 1.0]
  moving 0 [1] @?= [0.0]
  moving 4 [] @?= []
  
test22 :: TestTree
test22 =
  testGroup
  "Testing Block2 Task2 (SMA)"
  [ testCase "testing moving" testMoving ]



