{-# LANGUAGE RankNTypes #-}
module Task7 where

import Task6
import Lens.Micro
import System.Directory
import System.FilePath
import Data.Map.Strict (Map, (!), size, delete, elems, singleton, empty, fromList,
                        union, insert, member, notMember, lookup, assocs, keys)

ls :: Traversal' FS FilePath
ls = contentsDir . lens name (\struct nameStr -> struct{name = nameStr})

cd :: FilePath -> Traversal' FS FS
cd dir = contentsDir . filtered (\struct -> isDir struct && (name struct == dir))

file :: FilePath -> Traversal' FS FilePath
file need = failing (fileNameLens . filtered (==need)) 
                    (contentsDir . filtered (\struct -> (struct ^. fileNameLens) 
                         == need) . fileNameLens)