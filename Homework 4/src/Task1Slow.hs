module Task1Slow where

data Point = Point Int Int deriving (Show, Eq)

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
perimeter array = perimeterCycle (array ++ [head array])

perimeterCycle  :: [Point] -> Double
perimeterCycle [] = 0
perimeterCycle [alone] = 0
perimeterCycle (one:others) = distance one (head others) + perimeterCycle others

doubleArea  :: [Point] -> Int
doubleArea [] = 0
doubleArea [alone] = 0
doubleArea array = abs(doubleAreaCycle (array ++ [head array]))

doubleAreaCycle  :: [Point] -> Int
doubleAreaCycle [] = 0
doubleAreaCycle [alone] = 0
doubleAreaCycle (one:others) = crossProduct one (head others) + doubleAreaCycle others

distance :: Point -> Point -> Double
distance a b = sqrt (fromIntegral power2)
  where
    power2 = scalarProduct (minus b a) (minus b a)
