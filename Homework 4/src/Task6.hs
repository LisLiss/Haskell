module Task6 where

import Lens.Micro
import System.Directory
import System.FilePath
import Data.Map.Strict (Map, (!), size, delete, elems, singleton, empty, fromList,
                        union, insert, member, notMember, lookup, assocs, keys)

data FS
  = Dir
    { name     :: FilePath
    , contents :: [FS]
    }
  | File
    { name     :: FilePath
    }
    deriving (Show, Eq)

isDir :: FS -> Bool
isDir (Dir _ _) = True
isDir (File _) = False

--base lenses for Dir and File

contentsDir :: Traversal' FS FS
contentsDir f struct = if isDir struct then
                              Dir (name struct) <$> traverse f (contents struct)
                          else
                             pure struct

fileNameLens :: Traversal' FS FilePath
fileNameLens f struct = if isDir struct then
                             pure struct
                        else
                          fmap File (f (name struct))

dirNameLens :: Traversal' FS FilePath
dirNameLens f struct = if isDir struct then
                             fmap Dir (f (name struct)) <*> pure (contents struct)
                        else
                          pure struct

getDirectory :: FilePath -> IO FS
getDirectory path = do
  let foldersAndFilesIn = listDirectory path
  pathFoldersAndFiles <- map (path </>) <$> foldersAndFilesIn
  typeFoldersAndFiles <- mapM doesDirectoryExist pathFoldersAndFiles
  let folders = map snd (filter fst
                                (zip typeFoldersAndFiles pathFoldersAndFiles))
  let files = map snd (filter (not . fst)
                              (zip typeFoldersAndFiles pathFoldersAndFiles))
  dirsAns <- mapM getDirectory folders
  let filesAns = map (File . takeFileName) files
  return (Dir (takeFileName path) (filesAns ++ dirsAns))