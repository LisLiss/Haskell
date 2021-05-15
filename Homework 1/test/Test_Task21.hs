module Test_Task21 (testTask21) where

import Block1.Task3
import Block2.Task21
import Test.Tasty
import Test.Tasty.HUnit
import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..))


testFoldr :: Assertion
testFoldr = 
    assertEqual
    "Bug:"
    ([[1, 2, 2, 6, 7], [5], [2, 90], []])
    (map (foldr (mappend . (: [])) mempty) 
          [Node (2 :| [2]) (Node (1 :| []) List List) 
                           (Node (6 :| []) List (Node (7 :| []) List List)),
           Node (5 :| []) List List,
           Node (2 :| []) List (Node (90 :| []) List List),
           List
           ])

testFoldMap :: Assertion
testFoldMap =
    assertEqual
    "Bug:"
    ([[1, 2, 2, 6, 7], [5], [2, 90], []])
    (map (foldMap (: [])) 
          [Node (2 :| [2]) (Node (1 :| []) List List) 
                           (Node (6 :| []) List (Node (7 :| []) List List)),
           Node (5 :| []) List List,
           Node (2 :| []) List (Node (90 :| []) List List),
           List
           ])
    
    
testSortWithToList :: Assertion
testSortWithToList =
    assertEqual
    "Bug:"
    ([[1, 2, 2, 4, 7], [5], [6, 90], []])
    (map toList (map fromListToTree [[4, 2, 7, 1, 2], [5], [90, 6], []]))


testTask21 :: TestTree
testTask21 = 
    testGroup
    "Testing block 2 task 1 (instance foldable for Tree):"
    [ testCase "Testing foldr" testFoldr 
    , testCase "Testing foldMap" testFoldMap 
    , testCase "Testing sorting (toList . fromList = sort)" testSortWithToList 
    ]