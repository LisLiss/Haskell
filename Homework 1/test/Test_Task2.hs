module Test_Task2 (testTask2) where

import Block1.Task2
import Test.Tasty
import Test.Tasty.HUnit


testPlus :: Assertion
testPlus =
    assertEqual
    "Bug:"
    ([(1 :: Nat), (2 :: Nat), (1 :: Nat), (47 :: Nat)])
    ([(0 :: Nat) + ( 1 :: Nat), (1 :: Nat) + ( 1 :: Nat), 
      (1 :: Nat) + ( 0 :: Nat), (38 :: Nat) + ( 9 :: Nat)])

testMinus :: Assertion
testMinus =
    assertEqual
    "Bug:"
    ([(0 :: Nat), (0 :: Nat), (1 :: Nat), (29 :: Nat)])
    ([(0 :: Nat) - ( 1 :: Nat), (1 :: Nat) - ( 1 :: Nat), 
      (1 :: Nat) - ( 0 :: Nat), (38 :: Nat) - ( 9 :: Nat)])

testMul :: Assertion
testMul =
    assertEqual
    "Bug:"
    ([(0 :: Nat), (1 :: Nat), (0 :: Nat), (27 :: Nat)])
    ([(0 :: Nat) * ( 1 :: Nat), (1 :: Nat) * ( 1 :: Nat), 
      (1 :: Nat) * ( 0 :: Nat), (3 :: Nat) * ( 9 :: Nat)])

testFromInteger :: Assertion
testFromInteger =
    assertEqual
    "Bug:"
    ([Z, S Z, S (S Z), S ( S (S Z)), S (S ( S (S Z)))])
    (map fromInteger [0, 1, 2, 3, 4])
   
testFromNat :: Assertion
testFromNat =
    assertEqual
    "Bug:"
    ([0, 1, 2, 3, 4])
    (map fromNat [Z, S Z, S (S Z), S ( S (S Z)), S (S ( S (S Z)))])
   
testIsEven :: Assertion
testIsEven =
    assertEqual
    "Bug:"
    ([False, False, True, True])
    (map isEven [(3 :: Nat), (15 :: Nat), (4 :: Nat), (18 :: Nat)])
     
testDivNat :: Assertion
testDivNat =
    assertEqual
    "Bug:"
    ([0, 5, 4, 0])
    (map (uncurry divNat) [((3 :: Nat), (15 :: Nat)), ((16 :: Nat), (3 :: Nat)),
                  ((8 :: Nat), (2 :: Nat)), ((0 :: Nat), (5 :: Nat))])

testModNat :: Assertion
testModNat =
    assertEqual
    "Bug:"
    ([3, 1, 0, 0])
    (map (uncurry modNat) [((3 :: Nat), (15 :: Nat)), ((16 :: Nat), (3 :: Nat)),
                  ((8 :: Nat), (2 :: Nat)), ((0 :: Nat), (5 :: Nat))])

testTask2 :: TestTree
testTask2 = 
    testGroup
    "Testing block 1 task 2 (natural numbers):"
    [ testCase "Testing plus" testPlus 
    , testCase "Testing minus" testMinus 
    , testCase "Testing multiply" testMul
    , testCase "Testing from integer to nat" testFromInteger
    , testCase "Testing from nat to integer" testFromNat
    , testCase "Testing isEven" testIsEven
    , testCase "Testing division" testDivNat
    , testCase "Testing mod" testModNat
    ]