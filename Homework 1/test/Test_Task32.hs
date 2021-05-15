module Test_Task32 (testTask32) where

import Block3.Task32
import Test.Tasty
import Test.Tasty.HUnit
--import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..))

testAssociativityNonEmpty :: Assertion
testAssociativityNonEmpty = 
    assertEqual
    "Bug:"
    ((('a' :| []) <> ('b' :| [])) <> ('c' :| ['d']))
    (('a' :| []) <> (('b' :| []) <> ('c' :| ['d'])))
    
testAssociativityThisOrThat :: Assertion
testAssociativityThisOrThat = 
    assertEqual
    "Bug:"
    (((This "a") <> (That "b")) <> (This "c"))
    ((This "a") <> ((That "b") <> (This "c")))
        
testAssociativityName :: Assertion
testAssociativityName = 
    assertEqual
    "Bug:"
    (((Name "a") <> (Name "b")) <> (Name "c"))
    ((Name "a") <> ((Name "b") <> (Name "c")))

testAssociativityEndo :: Assertion
testAssociativityEndo = 
    assertEqual
    "Bug:"
    (getEndo (((Endo (\x -> x ++ "k")) <> (Endo (\x -> x ++ "e"))) <> (Endo (\x -> x ++ "k"))) "e")
    (getEndo ((Endo (\x -> x ++ "k")) <> (Endo ((\x -> x ++ "e")) <> (Endo (\x -> x ++ "k")))) "e")


testIdentityName :: Assertion
testIdentityName = 
    assertEqual
    "Bug:"
    (["empty", "empty"])
    ([("empty" <> mempty), (mempty <> "empty")])

testIdentityEndo :: Assertion
testIdentityEndo = 
    assertEqual
    "Bug:"
    (["ke", "ek"])
    ([((getEndo (Endo (\x -> x ++ "e")) "k") <> mempty),
       (mempty <> (getEndo (Endo (\x -> x ++ "k")) "e"))])

testTask32 :: TestTree
testTask32 = 
    testGroup
    "Testing block 3 task 2 (nonEmpty, ThisOrThat, Name, Endo):"
    [ testCase "Testing associativity law (semigroup) for nonEmpty" testAssociativityNonEmpty
    , testCase "Testing associativity law (semigroup) for ThisOrThat" testAssociativityThisOrThat
    , testCase "Testing associativity law (semigroup) for Name" testAssociativityName
    , testCase "Testing associativity law (semigroup) for Endo" testAssociativityEndo
    , testCase "Testing identity law (monoid) for Name" testIdentityName
    , testCase "Testing identity law (monoid) for Endo" testIdentityEndo
    ]