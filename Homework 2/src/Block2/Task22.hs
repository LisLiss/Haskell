module Block2.Task22 where

import Data.List
import Control.Monad.State

-- | Takes specified number of elements from given array
takeN :: Int -> [a] -> [a]
takeN 0 _ = []
takeN _ [] = []
takeN n a = (head a : takeN (n - 1) (tail a))

-- | State of SMA algorithm (watch Moving documentation)
move :: Real a => Fractional b => Int -> [a] -> State [a][b]
move x listA
  | length listA == 0 = return []
  | otherwise = do
    tailList <- get 
    let state = takeN x (head listA : tailList)
    let lenState = genericLength state
    let sumState = sum state
    let avg = if ((length state) == 0)
                then 0
              else realToFrac sumState / lenState
    let tailAns = evalState (move x (tail listA)) state
    return (avg : tailAns)
                  
-- | SMA algorithm for calculating average out of specified number of elements
moving :: Real a => Fractional b => Int -> [a] -> [b]
moving x listA = evalState (move x listA) []