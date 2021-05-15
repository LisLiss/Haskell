module Block3.Task31 where

import Control.Applicative

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  -- | Apply function to the answer of parser
  fmap f (Parser g) = Parser (fmap y . g)
    where
      y (a, s) = (f a, s)
  
instance Applicative (Parser s) where
  -- | Apply function contained in parser to the answer of second parser
  pure x = Parser (f)
    where
      f s = Just (x, s)
  (Parser f) <*> (Parser x) = Parser (g)
    where
      g s = do
          (resF, resTail) <- f s
          (resX, resTail2) <- x resTail
          Just ((resF resX), resTail2)      
 
instance Monad (Parser s) where
  -- | Monad instance for Parser
  (Parser f) >>= g = Parser (z)
      where
        z s = do
            (resF, resTail) <- f s
            let (Parser ff) = g resF
            ff resTail

instance Alternative (Parser s) where
  -- | Take one of parser (the first one which won`t fail)
  empty = Parser (const Nothing)            
  (Parser x) <|> (Parser y) = Parser (g)
    where
      g s = x s <|> y s
      
      