module Test_Task31 (testTask31) where

import Block3.Task31
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..))

testMaybeConcatNum :: Assertion
testMaybeConcatNum = 
    assertEqual
    "Bug:"
    ([[1, 2, 4, 8, 16], [32, 4, 10]])
    (map maybeConcat 
          [[Just [1, 2], Nothing, Just [4], Just [8, 16], Nothing]
          , [Just [32, 4, 10], Just []]
          ])
    
testMaybeConcatString :: Assertion
testMaybeConcatString = 
    assertEqual
    "Bug:"
    (["kek", "kekos", "kekosovich"])
    (maybeConcat [Nothing, Just["kek", "kekos"], Nothing, Nothing, Nothing, Just ["kekosovich"]])

testTask31 :: TestTree
testTask31 = 
    testGroup
    "Testing block 3 task 1 (concat monoids):"
    [ testCase "Testing maybeConcat with int" testMaybeConcatNum
    , testCase "Testing maybeConcat with string" testMaybeConcatString
    ]