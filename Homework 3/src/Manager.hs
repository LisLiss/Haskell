{-# LANGUAGE OverloadedStrings     #-}
module Manager where

import Command
import Folder
import File
import System.FilePath (splitDirectories, joinPath)
import System.Directory.Internal.Prelude (getArgs)
import System.Directory (getCurrentDirectory, Permissions, getPermissions, getModificationTime, readable,
                         listDirectory, doesDirectoryExist, getFileSize)
import Data.Map.Strict (Map, (!), size, delete, elems, singleton, empty, fromList,
                        union, insert, member, notMember, lookup, assocs, keys)
import Data.ByteString (readFile)
import Data.List (foldl')

-- | initialize root (from where we start)
rootInitialize :: IO String
rootInitialize = do
  args <- getArgs
  if (Prelude.length args) == 0 then
    getCurrentDirectory
  else
    return (head args)

-- | initialize files in root
filesInitialize :: FilePath -> IO File
filesInitialize path = do
  let pathToStrings = reverse (splitDirectories path)
  permissions <- getPermissions path
  time <- getModificationTime path
  size <- getFileSize path
  cont <- if readable permissions then
            Data.ByteString.readFile path else
              return ""
  return (File (head pathToStrings) (tail pathToStrings) permissions time cont size)

-- | initialize folders in root  
foldersInitialize :: FilePath -> IO (Map [String] Folder)
foldersInitialize path = do
  let pathToStrings = reverse (splitDirectories path)
  permissions <- getPermissions path
  time <- getModificationTime path
  size <- getFileSize path
  if (readable permissions) then do
    foldersAndFilesIn <- listDirectory path
    let pathFoldersAndFiles = map (:pathToStrings) foldersAndFilesIn
    typeFoldersAndFiles <- mapM isFolder pathFoldersAndFiles
    let folders = map snd (filter fst 
                                  (zip typeFoldersAndFiles (map (\x -> joinPath (reverse x)) pathFoldersAndFiles)))
    let files = map snd (filter (not . fst) 
                                (zip typeFoldersAndFiles (map (\x -> joinPath (reverse x)) pathFoldersAndFiles)))
    foldersRec <- mapM foldersInitialize folders
    filesRec <- mapM filesInitialize files
    let filesMap = Data.Map.Strict.fromList (map (\x -> (fileName x, x)) filesRec)
    let kek = map (reverse . splitDirectories) folders
    let foldersMap = Data.Map.Strict.fromList (map (\x -> (head x, x)) kek)
    return (Data.Map.Strict.insert pathToStrings
                                  (Folder (head pathToStrings) (tail pathToStrings) 
                                          permissions time filesMap foldersMap size)
                                  (foldl' Data.Map.Strict.union Data.Map.Strict.empty foldersRec))
  else
    return (Data.Map.Strict.singleton pathToStrings (Folder (head pathToStrings)
                                                            (tail pathToStrings)
                                                            permissions
                                                            time
                                                            Data.Map.Strict.empty
                                                            Data.Map.Strict.empty
                                                            0 ))
      where
        isFolder path = do
          let pathToFilePath = joinPath (reverse path)
          doesDirectoryExist pathToFilePath

-- | initialize system
managerInitialize :: FilePath -> IO NowState
managerInitialize path = do
  let pathToStrings = reverse (splitDirectories path)
  folders <- foldersInitialize path
  return (NowState pathToStrings (folders ! pathToStrings) pathToStrings folders)


