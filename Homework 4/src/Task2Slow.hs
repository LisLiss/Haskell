module Task2Slow where

import System.Random

integral :: Double -> Double -> (Double -> Double) -> Int -> IO Double
integral bot top function count = do
       gen <- newStdGen
       let rs = randomRs (bot, top) gen
       return (((top - bot) * sum (map function (take count rs))) / fromIntegral count)