module Block1.Task13 where

data NonEmpty a = a :| [a]
  deriving (Show, Eq)

-- | Functor instance for NonEmpty
instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

-- | Applicative instance for NonEmpty
instance Applicative NonEmpty where
  pure x = (:| []) x
  (x :| xs) <*> (y :| ys) = xy :| xys
    where
      (xy : xys) = (x : xs) <*> (y : ys)
 
-- | Monad instance for NonEmpty     
instance Monad NonEmpty where
  (x :| xs) >>= f = y :| (ys ++ end)
    where
      (y :| ys) = f x
      fromNonEmptyToList (x :| xs) = x : xs
      compose = (fromNonEmptyToList . f)
      end = xs >>= compose
  
-- | Foldable instance for NonEmpty    
instance Foldable NonEmpty where
  foldr f first (x :| xs) = f x (foldr f first xs)

-- | Traversable instance for NonEmpty
instance Traversable NonEmpty where
  traverse f (x :| xs) = (fmap (:|) (f x)) <*> (traverse f xs) 