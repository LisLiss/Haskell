{-# LANGUAGE InstanceSigs #-}

module Block3.Task32 where

data NonEmpty x =
    x :| [x]
    deriving (Eq, Show)

instance Semigroup (NonEmpty x) where
    -- |Semigroup with <> defined as concat of two Lists (NonEmpty lists)
    (<>) :: NonEmpty x -> NonEmpty x -> NonEmpty x
    (fF :| otherF) <> (fS :| otherS) = fF :| (otherF ++ (fS : otherS))

data ThisOrThat a b
    = This a
    | That b
    | Both a b
    deriving (Eq, Show)

instance (Semigroup x, Semigroup y) => Semigroup (ThisOrThat x y) where
    -- |Semigroup with <> defined as concat from Task 3.1 (This == Left and That == Right)
    (<>) :: ThisOrThat x y -> ThisOrThat x y -> ThisOrThat x y
    Both x y <> Both xx yy = Both (x <> xx) (y <> yy)
    Both x y <> This xx    = Both (x <> xx) y
    Both x y <> That yy    = Both x (y <> yy)
    This xx <> Both x y    = Both (xx <> x) y
    That yy <> Both x y    = Both x (yy <> y)
    This x <> This xx      = This (x <> xx)
    That y <> That yy      = That (y <> yy)
    This x <> That y       = Both x y
    That y <> This x       = Both x y


data Name
    = Empty
    | Name String
    deriving (Eq, Show)

instance Semigroup Name where
    -- |Monoid with <> defined as concat of two strings and empty string as mempty
    (<>) :: Name -> Name -> Name
    Name x <> Name y = Name (x ++ ('.' : y))
    Name x <> Empty  = Name x
    Empty <> Name y  = Name y

instance Monoid Name where
    mempty = Empty

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo x) where
    -- |Monoid with <> defined as composition of two function and id function as mempty
    Endo x <> Endo y = Endo (x . y)

instance Monoid (Endo x) where
    mempty = Endo id