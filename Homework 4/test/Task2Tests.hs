module Task2Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Task2Fast as F
import Task2Slow as S

testIntegral :: Assertion
testIntegral = do
    unwrapped <- S.integral 1 2 sin 1000000
    abs(unwrapped - 0.95645) < 0.1 @?= True
    unwrapped <- S.integral 1 2 cos 1000000
    abs(unwrapped - 0.067826) < 0.1 @?= True
    unwrapped <- S.integral 1 2 (\x -> cos x + x + x + 1 / x + x * x) 1000000
    abs(unwrapped - 6.0943) < 0.1 @?= True

tests2 :: TestTree
tests2 = testGroup "Tests task 2"
                    [testCase "Tests Integral" testIntegral]
