module Block3.Task32 where

import Block3.Task31

-- | Parser never fail and concume no input
ok :: Parser s ()
ok = return ()

-- | Check whether parser reach finish of stream
eof :: Parser s ()
eof = Parser f
  where
    f [] = Just ((), [])
    f x = Nothing

-- | Take predicate and consume element only if predicate True on it
satisfy :: (s -> Bool) -> Parser s s
satisfy pred = Parser f
  where
    f [] = Nothing
    f (x:xs) = if (pred x)
                 then Just (x, xs)
               else Nothing

-- | Take element and consume element from stream only if it the same element          
element ::  Eq s => s -> Parser s s
element x = satisfy (== x)

-- | Take [element] and consume [element] from stream only if it the same [element]          
stream ::  Eq s => [s] -> Parser s [s]
stream [] = Parser g
  where
    g s = Just ([], s)
stream x = Parser g
  where
    g s = do
      (resF, resX) <- runParser (element (head x)) s
      (resF2, resX2) <- runParser (stream (tail x)) resX
      Just (resF : resF2, resX2)  
      
