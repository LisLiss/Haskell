module Task6Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Task6
import Task7
import Lens.Micro
import System.Directory
import System.FilePath

sample :: FS
sample = Dir "1" [Dir "2" [File "3"], File "4", Dir "5" []]

testGetDirectory :: Assertion
testGetDirectory = do
       dir <- getCurrentDirectory
       fs <- Task6.getDirectory (dir </> "dir1")
       fs @?= Dir "dir1" [File "4", Dir "5" [File "6"], Dir "2" [File "3"]]
       fs <- Task6.getDirectory (dir </> "dir2")
       fs @?= Dir "dir2" [File "200000"]

tests6 :: TestTree
tests6 = testGroup "Tests task 6"
                    [testCase "Tests getDirectory" testGetDirectory]