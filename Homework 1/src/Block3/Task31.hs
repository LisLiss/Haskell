module Block3.Task31 where

import Data.Maybe (isJust, fromJust)
import Data.Either (isLeft, fromLeft, fromRight)

-- |Collect inner sublists from Just values
maybeConcat :: Monoid m => [Maybe m] -> m
maybeConcat list = foldMap f list
    where 
      f :: Monoid m => Maybe m -> m
      f x = if (isJust x)
               then (fromJust x)
            else
               mempty 
   
-- |Collect inner values of Either type        
eitherConcat :: Monoid x => Monoid y => [Either x y] -> (x, y)
eitherConcat list = foldMap f list
    where 
      f :: Monoid x => Monoid y => Either x y -> (x, y)
      f x = if (isLeft x)
               then ((fromLeft mempty x), mempty)
            else
               (mempty, (fromRight mempty x)) 
           
