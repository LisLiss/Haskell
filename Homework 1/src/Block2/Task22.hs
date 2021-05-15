module Block2.Task22 where

import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..), head, tail)

-- |Using fold split given list to sublists via separator
splitOn :: Eq x => [x] -> x -> NonEmpty [x]
splitOn list a = foldr f ([] :| []) list
    where
      f aCur (x :| y) = if a == aCur 
                           then [] :| (x : y)
                        else (aCur : x) :| y

-- |Using fold collect sublists to one list, where sublists divided with separator
joinWith :: NonEmpty [x] -> x -> [x]
joinWith list a = foldl f (DLNonEmpty.head list) (DLNonEmpty.tail list)
    where 
      f headCur tailCur = headCur ++ (a : tailCur)    
  