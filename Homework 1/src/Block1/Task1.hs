{-# LANGUAGE InstanceSigs #-}

module Block1.Task1 where

data Day = Mon
          | Tue
          | Wed
          | Thu
          | Fri
          | Sat
          | Sun
          deriving (Show)

-- | Calculate identical number of day
toNum :: Day -> Int
toNum Mon = 1
toNum Tue = 2
toNum Wed = 3
toNum Thu = 4
toNum Fri = 5
toNum Sat = 6
toNum Sun = 0

-- |Return day via its identical number
toDay :: Int -> Day
toDay 1 = Mon
toDay 2 = Tue
toDay 3 = Wed
toDay 4 = Thu
toDay 5 = Fri
toDay 6 = Sat
toDay 0 = Sun

-- |Calculate what day will come after given number of days after day given as argument
afterDays :: Day -> Int -> Day
afterDays nowDay num = toDay (mod (toNum nowDay + num) 7)

-- |Calculate what day will come after day given as argument
nextDay :: Day -> Day
nextDay nowDay = afterDays nowDay 1

-- |Return True if given day is weekend, False otherwise
isWeekend :: Day -> Bool
isWeekend nowDay = ((toNum nowDay) == 6) || ((toNum nowDay) == 0)

-- |Calculate count of days until next friday
daysToParty :: Day -> Int
daysToParty nowDay = mod (12 - toNum nowDay) 7

instance Eq Day where
    (==) :: Day -> Day -> Bool
    f == s = toNum f == toNum s


