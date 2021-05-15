{-# LANGUAGE BangPatterns #-}
module Task1Fast where

import Control.DeepSeq

data Point = Point !Int !Int deriving (Show, Eq)

instance NFData Point where
  rnf (Point x y) = seq (rnf x) (rnf y)

plus :: Point -> Point -> Point
plus (Point a b) (Point c d) = Point (a + c) (b + d)

minus :: Point -> Point -> Point
minus (Point a b) (Point c d) = Point (a - c) (b - d)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point a b) (Point c d) = (a * c) + (b * d)

crossProduct  :: Point -> Point -> Int
crossProduct (Point a b) (Point c d) = (a * d) - (b * c)

perimeter  :: [Point] -> Double
perimeter [] = 0
perimeter [alone] = 0
perimeter array = perimeterCycle (array ++ [head array]) 0

perimeterCycle  :: [Point] -> Double -> Double
perimeterCycle [] !acc = acc
perimeterCycle [alone] !acc = acc
perimeterCycle (one:others) !acc = perimeterCycle others (acc + distance one (head others))

doubleArea  :: [Point] -> Int
doubleArea [] = 0
doubleArea [alone] = 0
doubleArea array = abs(doubleAreaCycle (array ++ [head array]) 0)

doubleAreaCycle  :: [Point] -> Int -> Int
doubleAreaCycle [] !acc = acc
doubleAreaCycle [alone] !acc = acc
doubleAreaCycle (one:others) !acc = doubleAreaCycle others (acc + crossProduct one (head others))

distance :: Point -> Point -> Double
distance a b = sqrt (fromIntegral power2)
  where
    power2 = scalarProduct (minus b a) (minus b a)
