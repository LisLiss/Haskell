module Block1.Task12 where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)
  
-- | Functor instance for Tree
instance Functor Tree where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- | Applicative instance for Tree
instance Applicative Tree where
  pure x = Leaf x
  Leaf f <*> Leaf x = Leaf (f x)
  Branch f g <*> Branch x y = Branch (f <*> x) (g <*> y)
  leafF@(Leaf f) <*> Branch x y = Branch (leafF <*> x) (leafF <*> y)
  Branch f g <*> leafX@(Leaf x) = Branch (f <*> leafX) (g <*> leafX)
  
-- | Foldable instance for Tree
instance Foldable Tree where
  foldr f first (Leaf x) = f x first
  foldr f first (Branch x y) = foldr f (foldr f first y) x
  
-- | Traversable instance for Tree
instance Traversable Tree where
  traverse f (Leaf x) = fmap Leaf (f x)
  traverse f (Branch x y) = (fmap Branch (traverse f y)) <*> (traverse f x)