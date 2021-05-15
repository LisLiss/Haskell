module Task7Tests where

import Test.Tasty
import Test.Tasty.HUnit

import Task6
import Task7
import Lens.Micro

sample :: FS
sample = Dir "1" [Dir "2" [File "3"], File "4", Dir "5" []]

testCd :: Assertion
testCd = do
   sample ^? cd "2" @?= Just (Dir "2" [File "3"])
   sample ^? cd "3" @?= Nothing
   sample ^? cd "5" @?= Just (Dir "5" [])

testLs :: Assertion
testLs = do
   sample ^.. ls @?= ["2", "4", "5"]
   Dir "2" [Dir "200" []] ^.. ls @?= ["200"]
   Dir "2" [Dir "200" [File "4"]] ^.. ls @?= ["200"]

testFile :: Assertion
testFile = do
   File "200" ^? file "200" @?= Just "200"
   File "200" ^? file "1" @?= Nothing
   Dir "200" [File "1"] ^? file "10" @?= Nothing
   File "210000" ^? file "210000" @?= Just "210000"

tests7 :: TestTree
tests7 = testGroup "Tests task 7"
                    [testCase "Tests cd" testCd
                   , testCase "Tests ls" testLs
                   , testCase "Tests file" testFile]
