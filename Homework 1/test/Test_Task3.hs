module Test_Task3 (testTask3) where

import Block1.Task3
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..))

testFromListToTree :: Assertion
testFromListToTree =
    assertEqual
    "Bug:"
    ([Node (2 :| [2]) (Node (1 :| []) List List) (Node (6 :| []) List (Node (7 :| []) List List)),
    Node (2 :| []) List (Node (5 :| []) List List), List])
    (map fromListToTree [ [1, 7, 6, 2, 2], [5, 2], []])

testIsEmpty :: Assertion
testIsEmpty =
    assertEqual
    "Bug:"
    ([True, False, False])
    (map isEmptyTree [ List, (Node (5 :| []) List List),
                   (Node (4 :| []) List (Node (7 :| []) List List))] )

testSize :: Assertion
testSize =
    assertEqual
    "Bug:"
    ([0, 1, 5])
    (map sizeTree [ List, (Node (5 :| []) List List),
                       (fromListToTree [1, 7, 6, 2, 2])])

testFindValue :: Assertion
testFindValue =
    assertEqual
    "Bug:"
    ([False, True, True, False])
    (map (uncurry findValue) [(List, 4), ((Node (5 :| []) List List), 5),
                              ((fromListToTree [1, 7, 6, 2, 2]), 2), 
                              ((fromListToTree [3, 2, 50, 4, 11]), 15) 
                             ])


testInsertValue :: Assertion
testInsertValue =
    assertEqual
    "Bug:"
    ([Node (4 :| []) List List,
      Node (5 :| [5]) List List,
      Node (2 :| [2,2]) (Node (1 :| []) List List) 
                        (Node (6 :| []) List (Node (7 :| []) List List)),
      Node (4 :| []) (Node (2 :| []) List (Node (3 :| []) List List)) 
                     (Node (11 :| []) List (Node (50 :| []) (Node (15 :| []) List List) List))])
    (map (uncurry insertValue) [(List, 4), ((Node (5 :| []) List List), 5),
                              ((fromListToTree [1, 7, 6, 2, 2]), 2), 
                              ((fromListToTree [3, 2, 50, 4, 11]), 15) 
                             ])

testEraseValue :: Assertion
testEraseValue =
    assertEqual
    "Bug:"
    ( [Node (4 :| []) List List,
       Node (5 :| [5]) List List,
       Node (1 :| []) List (Node (2 :| [2,2]) List List),
       Node (3 :| []) (Node (2 :| []) List List) 
                      (Node (11 :| []) List (Node (50 :| []) (Node (15 :| []) List List) List))])
    (map (uncurry insertValue) [(List, 4), ((Node (5 :| []) List List), 5),
                              ((fromListToTree [1, 2, 2]), 2), 
                              ((fromListToTree [3, 2, 50, 11]), 15) 
                             ])

testTask3 :: TestTree
testTask3 = 
    testGroup
    "Testing block 1 task 3 (binary tree):"
    [ testCase "Testing from list to tree" testFromListToTree 
    , testCase "Testing isEmpty" testIsEmpty 
    , testCase "Testing size" testSize 
    , testCase "Testing findValue" testFindValue 
    , testCase "Testing insertValue" testInsertValue 
    , testCase "Testing eraseValue" testEraseValue 
    ]