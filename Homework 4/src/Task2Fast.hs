{-# LANGUAGE Strict #-}
module Task2Fast where

import System.Random
import Data.List
import Control.Monad (mapM, forM)
import Control.Concurrent.Thread
import Control.Monad.Par.Combinator
import Control.Monad.Par

integral :: Double -> Double -> (Double -> Double) -> Int -> IO Double
integral bot top function count = do
   lst1 <- randomList(bot, top)
   let lst = take count lst1
   let res = runPar $ parMapReduceRangeThresh 100 (InclusiveRange 1 count)
                         (return . function . fromIntegral)
                         (\x y -> return (x+y))
                          0
   return ((top - bot) / fromIntegral count * res)

randomList :: (Double, Double) -> IO [Double]
randomList interval =
  unfoldr (Just . randomR interval) <$> newStdGen