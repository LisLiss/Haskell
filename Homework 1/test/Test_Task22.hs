module Test_Task22 (testTask22) where

import Block2.Task22
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..))

testSplitOn :: Assertion
testSplitOn = 
    assertEqual
    "Bug:"
    ([("kek" :| ["kekos", "kekosovich"]), ("NothingTo,Split" :| []), ("f" :| [""]), ("r" :| ["r"])])
    (map (uncurry splitOn) 
          [("kek,kekos,kekosovich", ','), ("NothingTo,Split", '.'), ("f/", '/'), ("r/r", '/')])

testJoinWith :: Assertion
testJoinWith = 
    assertEqual
    "Bug:"
    (["kek,kekos,kekosovich", "NothingTo,Split", "f/", "r/r"])
    (map (uncurry joinWith) 
          [(("kek" :| ["kekos", "kekosovich"]), ',')
           , (("NothingTo,Split" :| []), '.')
           , (("f" :| [""]), '/'), (("r" :| ["r"]), '/')])

testJoinThenSplit :: Assertion
testJoinThenSplit =
    assertEqual
    "Bug:"
    ([("kek" :| ["kekos", "kekosovich"]), ("NothingTo,Split" :| []), ("f" :| [""]), ("r" :| ["r"])])
    (map (uncurry splitOn) 
          [((joinWith ("kek" :| ["kekos", "kekosovich"]) ','), ',') 
           , ((joinWith ("NothingTo,Split" :| []) '.'), '.') 
           , ((joinWith ("f" :| [""]) '/'), '/') 
           , ((joinWith ("r" :| ["r"]) '/'), '/') 
           ])


testTask22 :: TestTree
testTask22 = 
    testGroup
    "Testing block 2 task 2 (instance foldable for Tree):"
    [ testCase "Testing splitOn" testSplitOn
    , testCase "Testing joinWith" testJoinWith
    , testCase "Testing joinWith x . splitOn x = id" testJoinThenSplit
    ]