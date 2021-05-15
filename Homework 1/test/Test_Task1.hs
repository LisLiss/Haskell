module Test_Task1 (testTask1) where

import Block1.Task1
import Test.Tasty
import Test.Tasty.HUnit


testToNum :: Assertion
testToNum =
    assertEqual
    "Bug:"
    ([1, 2, 3, 4, 5, 6, 0])
    (map toNum [Mon, Tue, Wed, Thu, Fri, Sat, Sun])
         
testToDay :: Assertion
testToDay =
    assertEqual
    "Bug:"
    ([Mon, Tue, Wed, Thu, Fri, Sat, Sun])
    (map toDay [1, 2, 3, 4, 5, 6, 0])
         
testNextDay :: Assertion
testNextDay =
    assertEqual
    "Bug:"
    ([Tue, Wed, Thu, Fri, Sat, Sun, Mon])
    (map nextDay [Mon, Tue, Wed, Thu, Fri, Sat, Sun])
    
testAfterDay :: Assertion
testAfterDay =
    assertEqual
    "Bug:"
    ([Thu, Sat, Sun, Wed])
    (map (uncurry afterDays) [(Mon, 3), (Thu, 100), (Sun, 0), (Thu, 1^7 + 5)])

testIsWeekend :: Assertion
testIsWeekend =
    assertEqual
    "Bug:"
    ([False, False, False, False, False, True, True])
    (map isWeekend [Mon, Tue, Wed, Thu, Fri, Sat, Sun])

testDaysToParty :: Assertion
testDaysToParty =
    assertEqual
    "Bug:"
    ([4, 3, 2, 1, 0, 6, 5])
    (map daysToParty [Mon, Tue, Wed, Thu, Fri, Sat, Sun])

testTask1 :: TestTree
testTask1 = 
    testGroup
    "Testing block 1 task 1 (days):"
    [ testCase "Testing NextDay" testNextDay 
    , testCase "Testing AfterDays" testAfterDay
    , testCase "Testing toDay" testToDay
    , testCase "Testing toNum" testToNum
    , testCase "Testing isWeekend" testIsWeekend
    , testCase "Testing daysToParty" testDaysToParty
    ]