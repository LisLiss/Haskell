module Task3Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Task3
import Control.Concurrent.Thread
import Control.Exception.Base
import Control.Concurrent

testWorkability :: Assertion
testWorkability = do
     cht <- newCHT
     putCHT "a" 1 cht
     putCHT "b" 2 cht
     putCHT "c" 3 cht
     putCHT "d" 3 cht
     size <- sizeCHT cht
     size @?= 4
     value1 <- getCHT "a" cht
     value3 <- getCHT "c" cht
     value1 @?= Just 1
     value3 @?= Just 3
     n <- getCHT "n" cht
     n @?= Nothing

testParallelAsync :: Assertion
testParallelAsync = do
    cht <- newCHT
    (id, ans) <- Control.Concurrent.Thread.forkIO (do
      threadDelay 20
      mapM_ (\elem -> putCHT ("key" ++ show elem) ("elem" ++ show elem) cht) [1..20]
     )
    threadDelay 20
    throwTo id ThreadKilled
    temp <- ans
    handle handler (Control.Concurrent.Thread.result temp)
    mapped <- mapM (\elem -> getCHT ("key" ++ show elem) cht) [1..20]
    mapM_ (\(ind, elem) -> case (ind, elem) of
                             (_, Nothing) -> 1 @?= 1
                             (ind2, elem2) -> (ind, elem) @?= (ind2, Just ("elem" ++ show ind2))
                             ) (zip [1..20] mapped)

handler :: AsyncException -> IO()
handler e = putStr ""

tests3 :: TestTree
tests3 = testGroup "Tests task 3"
          [testCase "Tests All functions" testWorkability
         , testCase "Tests Stable on async exceptions and test parallel work" testParallelAsync]