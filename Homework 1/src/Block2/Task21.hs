{-# LANGUAGE InstanceSigs #-}

module Block2.Task21 where

import Block1.Task3

instance Foldable Tree where
    -- |Right-associative fold of a structure.
    foldr :: (x -> a -> a) -> a -> Tree x -> a
    foldr f x List = x
    foldr f x (Node node left right) = foldr f (foldr f (foldr f x right) node) left

    -- |Map each element of the structure to a monoid, and combine the results.
    foldMap :: Monoid m => (x -> m) -> Tree x -> m
    foldMap f List = mempty
    foldMap f (Node node left right) = (foldMap f left) <> (foldMap f node) <> (foldMap f right)
