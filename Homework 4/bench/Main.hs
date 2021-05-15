module Main where

import Task1Fast as F1
import Task1Slow as S1
import Task2Fast as F2
import Task2Slow as S2
import Task3
import Criterion.Main
import Control.Concurrent.Thread

main :: IO()
main = defaultMain [bgroup "Benchs Task 1" [fastPer0, fastPer1, slowPer1, fastSq0, fastSq1, slowSq1]
                  , bgroup "Benchs Task 2" [fast2, slow2]
                  , bgroup "Benchs Task 3" [cht]]

fastPer0 :: Benchmark
fastPer0 = bench "Fast version Task1 perimetr 10^7 ops" (nf F1.perimeter (map (\x -> F1.Point x (x + 1)) [1..10000000]))

fastPer1 :: Benchmark
fastPer1 = bench "Fast version Task1 perimetr 10^5 ops" (nf F1.perimeter (map (\x -> F1.Point x (x + 1)) [1..100000]))

slowPer1 :: Benchmark
slowPer1 = bench "Slow version Task1 perimetr 10^5 ops" (nf S1.perimeter (map (\x -> S1.Point x (x + 1)) [1..100000]))

fastSq0 :: Benchmark
fastSq0 = bench "Fast version Task1 doubleArea 10^7 ops" (nf F1.doubleArea (map (\x -> F1.Point x (x + 1)) [1..10000000]))

fastSq1 :: Benchmark
fastSq1 = bench "Fast version Task1 doubleArea 10^5 ops" (nf F1.doubleArea (map (\x -> F1.Point x (x + 1)) [1..100000]))

slowSq1 :: Benchmark
slowSq1 = bench "Slow version Task1 doubleArea 10^5 ops" (nf S1.doubleArea (map (\x -> S1.Point x (x + 1)) [1..100000]))

fast2 :: Benchmark
fast2 = bench "Fast version Task2 integral 10^6 ops" (nfIO (F2.integral 1 100  (\x -> x*x*x*x + cos x + sin x + 1/(x * x) + tan x)  1000000))

slow2 :: Benchmark
slow2 = bench "Slow version Task2 integral 10^6 ops" (nfIO (S2.integral 1 100  (\x -> x*x*x*x + cos x + sin x + 1/(x * x) + tan x) 1000000))
cht :: Benchmark
cht = bench "Task3 cht" (nfIO stress)
  where
    stress :: IO()
    stress = do
      cht <- newCHT
      (id, ans) <- Control.Concurrent.Thread.forkIO (
            mapM_ (\elem -> putCHT ("key" ++ show elem) ("elem" ++ show elem) cht) [1..30000]
           )
      temp <- ans
      mapped <- mapM (\elem -> getCHT ("key" ++ show elem) cht) [1..30000]
      mapM_ (\(ind, elem) -> case (ind, elem) of
                                   (_, Nothing) -> return()
                                   (ind2, elem2) -> return ()
                                   ) (zip [1..30000] mapped)
      