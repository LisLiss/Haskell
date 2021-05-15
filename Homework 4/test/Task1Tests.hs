module Task1Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Task1Fast as F
import Task1Slow as S

testPlus :: Assertion
testPlus = do
   S.plus (S.Point 5 5) (S.Point 10 10) @?= S.Point 15 15
   S.plus (S.Point 0 5) (S.Point 1 10) @?= S.Point 1 15
   S.plus (S.Point 5 0) (S.Point 10 1) @?= S.Point 15 1
   S.plus (S.Point (-5) 5) (S.Point 10 10) @?= S.Point 5 15

testMinus :: Assertion
testMinus = do
   S.minus (S.Point 5 5) (S.Point (-10) (-10)) @?= S.Point 15 15
   S.minus (S.Point 0 5) (S.Point (-1) (-10)) @?= S.Point 1 15
   S.minus (S.Point 5 0) (S.Point (-10) (-1)) @?= S.Point 15 1
   S.minus (S.Point (-5) 5) (S.Point (-10) (-10)) @?= S.Point 5 15

testCrossProduct :: Assertion
testCrossProduct = do
   S.crossProduct (S.Point 5 5) (S.Point 10 10) @?= 0
   S.crossProduct (S.Point 0 5) (S.Point 1 10) @?= -5
   S.crossProduct (S.Point 5 0) (S.Point 10 1) @?= 5
   S.crossProduct (S.Point (-5) 5) (S.Point 10 10) @?= -100

testScalar :: Assertion
testScalar = do
   S.scalarProduct (S.Point 5 5) (S.Point 10 10) @?= 100
   S.scalarProduct (S.Point 0 5) (S.Point 1 10) @?= 50
   S.scalarProduct (S.Point 5 0) (S.Point 10 1) @?= 50
   S.scalarProduct (S.Point (-5) 5) (S.Point 10 10) @?= 0


testSlowPerimeter :: Assertion
testSlowPerimeter = do
    S.perimeter [S.Point 0 0, S.Point 1 0, S.Point 1 1, S.Point 0 1] @?= 4
    S.perimeter [S.Point 0 0] @?= 0
    S.perimeter [] @?= 0
    S.perimeter [S.Point 0 0, S.Point 1 0] @?= 2

testFastPerimeter :: Assertion
testFastPerimeter = do
    F.perimeter [F.Point 0 0, F.Point 1 0, F.Point 1 1, F.Point 0 1] @?= 4
    F.perimeter [F.Point 0 0] @?= 0
    F.perimeter [] @?= 0
    F.perimeter [F.Point 0 0, F.Point 1 0] @?= 2

testSlowSq :: Assertion
testSlowSq = do
    S.doubleArea [S.Point 0 0] @?= 0
    S.doubleArea [] @?= 0
    S.doubleArea [S.Point 0 0, S.Point 1 0, S.Point 1 1, S.Point 0 1] @?= 2
    S.doubleArea [S.Point 0 0, S.Point 1 0] @?= 0

testFastSq :: Assertion
testFastSq = do
    F.doubleArea [F.Point 0 0] @?= 0
    F.doubleArea [] @?= 0
    F.doubleArea [F.Point 0 0, F.Point 1 0, F.Point 1 1, F.Point 0 1] @?= 2
    F.doubleArea [F.Point 0 0, F.Point 1 0] @?= 0

tests1 :: TestTree
tests1 = testGroup
         "Tests task 1"
         [testCase "Tests Slow Perimeter" testSlowPerimeter
        , testCase "Tests Fast Perimeter" testFastPerimeter
        , testCase "Tests Slow Square" testSlowSq
        , testCase "Tests Fast Square" testFastSq
        , testCase "Tests Plus" testPlus
        , testCase "Tests Minus" testMinus
        , testCase "Tests Scalar product" testScalar
        , testCase "Tests Cross product" testCrossProduct              
        ]