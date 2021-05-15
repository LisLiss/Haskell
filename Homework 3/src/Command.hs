{-# LANGUAGE OverloadedStrings #-}

module Command where
import System.FilePath (splitDirectories, combine, joinPath, addTrailingPathSeparator, takeExtension)
import System.Directory.Internal (Permissions (..), writable)
import Data.Maybe (fromJust)
import System.Directory.Internal.Prelude (getArgs)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Map.Strict (Map, (!), size, delete, elems, singleton, empty, fromList, 
                        union, insert, member, notMember, lookup, assocs, keys, (!?))
import Data.ByteString (readFile, writeFile, empty, length)
import Data.Time.Clock (UTCTime)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow ((***))
import Control.Monad (join, when, void)
import Control.Monad.State
import Control.Monad.Writer.Strict
import System.Directory
import Data.List (partition, foldl')
import Folder
import File
import Error

-- | data with all commands in file-manager
data Commands =
    Cd FilePath
  | Dir
  | Ls FilePath
  | CreateFolder String
  | CreateFile String
  | Cat FilePath
  | RemoveFolder FilePath
  | RemoveFile FilePath
  | IsEmpty FilePath
  | Write FilePath ByteString
  | Find FilePath String
  | Info FilePath
  | Save
  | Exit

data NowState = NowState {
    nowStatePath :: [String]
  , nowStateFolder :: Folder
  , nowStateRoot :: [String]
  , nowStateFileSystem :: Map [String] Folder
} deriving (Eq, Show)

type NewState = ExceptT Errors (ReaderT UTCTime (WriterT ByteString (State NowState)))

-- | help for makePathFromTwo
makePathFromTwoRec :: [String] -> [String] -> [String]
makePathFromTwoRec beforePath (".":xs) = makePathFromTwoRec beforePath xs
makePathFromTwoRec (beforePath:xs) ("..":xss) = makePathFromTwoRec xs xss
makePathFromTwoRec beforePath insidePath@("/":xs) = reverse insidePath
makePathFromTwoRec beforePath (x:xs) = makePathFromTwoRec (x : beforePath) xs
makePathFromTwoRec beforePath _ = beforePath

-- | make one path from two
makePathFromTwo :: [String] -> [String] -> [String]
makePathFromTwo fst snd = makePathFromTwoRec fst (reverse snd)

-- | command cd
cdCommand :: [String] -> Folder -> NewState()
cdCommand path folder = do
  modify (\curState -> curState{nowStatePath = path, nowStateFolder = folder})

-- | help for createFolderCommand and createFileCommand
-- | update information in parents
updateParent :: [String] -> UTCTime -> NewState()
updateParent path time = do
  newStateFileSystem <- gets nowStateFileSystem
  newStatePath <- gets nowStatePath
  if (member path newStateFileSystem) then do
    let updatedFolder = (fromJust (Data.Map.Strict.lookup path newStateFileSystem)) {folderTime = time}
    modify (\curState -> curState{nowStateFileSystem = insert path updatedFolder newStateFileSystem,
                                  nowStateFolder = (insert path updatedFolder newStateFileSystem) ! newStatePath})
    updateParent (folderPath updatedFolder) time
  else
    return ()

-- | command create-folder
createFolderCommand :: Folder -> [String] -> String -> NewState Folder
createFolderCommand folder path name =
  if writable (folderPermissions folder) then do
    let updatedFolder = folder {folderFoldersIn = insert name (makePathFromTwo path [name]) (folderFoldersIn folder)}
    newTime <- ask
    let newFolder = Folder name path (Permissions True True False False) 
                           newTime Data.Map.Strict.empty Data.Map.Strict.empty 0
    newStateFileSystem <- gets nowStateFileSystem
    modify (\curState ->
             curState{nowStateFileSystem = insert (makePathFromTwo path [name])
                                                  newFolder
                                                  (insert path updatedFolder newStateFileSystem)})
    updateParent path newTime
    return newFolder
  else
    throwError (ErrorCreate (reverse path) "can't write in this folder")
  
-- | command create-file
createFileCommand :: Folder -> [String] -> String -> NewState File
createFileCommand folder path name =
  if writable (folderPermissions folder) then do
    newTime <- ask
    let file = File name path (Permissions True True False False) newTime Data.ByteString.empty 0
    let updatedFolder = folder {folderFilesIn = insert name file (folderFilesIn folder)}
    newStateFileSystem <- gets nowStateFileSystem
    modify (\curState ->
             curState{nowStateFileSystem = insert path
                                                  updatedFolder
                                                  newStateFileSystem})
    updateParent path newTime
    return file
  else
    throwError (ErrorCreate (reverse path) "can't create file in this folder")
  
-- | command cat  
catCommand :: File -> NewState()
catCommand file = tell (fileContent file)

-- | command remove-folder
removeFolderCommand :: [String] -> Folder -> NewState()
removeFolderCommand path folder =
  if (size (folderFilesIn folder) == 0) && (size (folderFoldersIn folder) == 0) then do
    nowPath <- gets nowStatePath
    if nowPath /= path then do
      newTime <- ask
      newStateFileSystem <- gets nowStateFileSystem
      let updatedFolder = (newStateFileSystem ! (folderPath folder)) {
                           folderFoldersIn = delete 
                                             (folderName folder)
                                             (folderFoldersIn (newStateFileSystem ! (folderPath folder)))}
      modify (\curState ->
               curState{nowStateFileSystem = delete path
                                                    (insert (folderPath folder) updatedFolder newStateFileSystem)})
      updateParent path newTime
    else
      throwError (ErrorRemoveFolder (reverse path) "can't remove current folder")
  else 
    throwError (ErrorRemoveFolder (reverse path) "can't remove non-empty folder")

-- | command remove-file
removeFileCommand :: [String] -> File -> NewState()
removeFileCommand path file = do
  newTime <- ask
  newStateFileSystem <- gets nowStateFileSystem
  let updatedFolder = (newStateFileSystem ! (filePath file)) {
                       folderFilesIn = delete 
                                         (fileName file)
                                         (folderFilesIn (newStateFileSystem ! (filePath file)))}
  modify (\curState ->
           curState{nowStateFileSystem = delete path
                                                (insert (filePath file) updatedFolder newStateFileSystem)})
  updateParent path newTime

-- | check empty file for command is-empty
isEmptyFileCommand :: File -> NewState()
isEmptyFileCommand file = 
  if (fileSize file) == 0 then
    tell ("empty")
  else
    tell ("non-empty")

-- | check empty folder for command is-empty
isEmptyFolderCommand :: Folder -> NewState()
isEmptyFolderCommand folder = 
  if ((size (folderFoldersIn folder)) + (size (folderFoldersIn folder))) == 0 then
    tell ("empty")
  else
    tell ("non-empty")
  
-- | command write
writeCommand :: ByteString -> [String] -> File -> NewState()
writeCommand content path file = 
  if writable (filePermissions file) then do
    newTime <- ask
    newStateFileSystem <- gets nowStateFileSystem
    let updatedFolder = (newStateFileSystem ! (filePath file)) {
                         folderFilesIn = insert 
                                           (fileName file)
                                           (file {fileContent = content, 
                                                  fileSize = toInteger(Data.ByteString.length content),
                                                  fileTime = newTime})
                                           (folderFilesIn (newStateFileSystem ! (filePath file)))}
    modify (\curState ->
             curState{nowStateFileSystem = insert (filePath file) updatedFolder newStateFileSystem})
    updateParent path newTime
  else 
    throwError(ErrorWriteFile (reverse path) "can't write in non-writable file")
 
-- | command find 
recursiveFindCommand :: String -> Folder -> FilePath -> NewState()
recursiveFindCommand name folder path = do
  mapM_ checkName (keys (folderFilesIn folder))
  newStateFileSystem <- gets nowStateFileSystem
  mapM_ (recFind newStateFileSystem) (assocs (folderFoldersIn folder))
    where 
      checkName :: String -> NewState()
      checkName curName
        | curName == name = tell (pack ((combine path curName) <> "\n"))
        | curName /= name = tell ( pack (""))
      recFind newStateFileSystem (newFolder, newPath) = do
        checkName newFolder
        recursiveFindCommand name (newStateFileSystem ! newPath) (combine path newFolder)

-- | command dir
dirCommand :: NewState()
dirCommand = lsCommand ""

-- | command ls
lsCommand :: FilePath -> NewState()
lsCommand path = do
  newStatePath <- gets nowStatePath
  let newPath = makePathFromTwo newStatePath (reverse (splitDirectories path))
  newStateFileSystem <- gets nowStateFileSystem
  let folder = newStateFileSystem ! newPath
  tell "\nFolders:\n"
  mapM_ printAll (keys (folderFoldersIn folder))
  tell "\nFiles:\n"
  mapM_ printAll (keys (folderFilesIn folder))
 where
   printAll :: String -> NewState()
   printAll x = tell (pack x <> "\n")

-- | check file info for command info
infoFileCommand :: File -> NewState()
infoFileCommand file = do
  let pathText = "\nPath:\n"
  let permissionsText = "\nPermissions:\n"
  let typeText = "\nType:\n"
  let timeText = "\nTime:\n"
  let sizeText = "\nSize:\n"
  let pathF = pack (joinPath (reverse (filePath file)))
  let permissionsF = pack (show (filePermissions file))
  let typeF =  pack (takeExtension (fileName file))
  let timeF =  pack (take 19 (show (fileTime file)))
  let sizeF =  pack (show (fileSize file))
  tell (pack pathText <> pathF
        <> permissionsText <> permissionsF
        <> typeText <> typeF
        <> timeText <> timeF
        <> sizeText <> sizeF)

-- | check folder info for command info
infoFolderCommand :: Folder -> NewState()
infoFolderCommand folder = do
  let pathText = "\nPath:\n"
  let permissionsText = "\nPermissions:\n"
  let sizeText = "\nSize:\n"
  let sizeFilesText = "\nNumber files in:\n"
  let pathF = pack (joinPath (reverse (folderPath folder)))
  let permissionsF = pack (show (folderPermissions folder))
  let sizeFilesF = pack (show ((size (folderFoldersIn folder)) + (size (folderFoldersIn folder))))
  let sizeF = pack (show (folderSize folder))
  tell (pack pathText <> pathF
        <> permissionsText <> permissionsF
        <> sizeText <> sizeF
        <> sizeFilesText <> sizeFilesF)

-- | helper for applyCommand (Create path)    
applyCreateCommand :: FilePath -> (Folder -> [String] -> String -> NewState()) -> NewState()
applyCreateCommand path createSmth = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
      throwError (ErrorCreate [path] "have folder with this name")
  else
    if member (tail previousPath) newStateFileSystem then do
      let mapFolder = folderFilesIn (newStateFileSystem ! (tail previousPath))
      if member (head previousPath) mapFolder then
        throwError (ErrorCreate [path] "don't have this file")
      else
        createSmth (newStateFileSystem ! (tail previousPath)) (tail previousPath) (head previousPath)
    else
        throwError (ErrorCreate [path] "don't have this folder")

-- | apply function from commands if possible
applyCommand :: Commands -> NewState()

-- | check errors and apply cat if has file
applyCommand (Cat path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
      throwError (ErrorNoSuchFile [path] "don't have this file")
  else
    if member (tail previousPath) newStateFileSystem then do
      let mapFolder = folderFilesIn (newStateFileSystem ! (tail previousPath))
      if member (head previousPath) mapFolder then
        catCommand (mapFolder ! (head previousPath))
      else
        throwError (ErrorNoSuchFile [path] "don't have this file")
    else
      throwError (ErrorNoSuchFile [path] "don't have this folder")

-- | check errors and apply remove if has file
applyCommand (RemoveFile path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
      throwError (ErrorRemoveFile [path] "don't have this file")
  else
    if member (tail previousPath) newStateFileSystem then do
      let mapFolder = folderFilesIn (newStateFileSystem ! (tail previousPath))
      if member (head previousPath) mapFolder then
        removeFileCommand previousPath (mapFolder ! (head previousPath))
      else
        throwError (ErrorRemoveFile [path] "don't have this file")
    else
      throwError (ErrorRemoveFile [path] "don't have this file")


-- | check errors and apply remove if has folder
applyCommand (RemoveFolder path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
    removeFolderCommand previousPath (newStateFileSystem !previousPath)
  else
    throwError (ErrorRemoveFolder [path] "don't have this folder")


-- | check errors and apply remove if has file
applyCommand (IsEmpty path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
      isEmptyFolderCommand (newStateFileSystem ! previousPath)
  else
    if member (tail previousPath) newStateFileSystem then do
      let mapFolder = folderFilesIn (newStateFileSystem ! (tail previousPath))
      if member (head previousPath) mapFolder then
        isEmptyFileCommand (mapFolder ! (head previousPath))
      else
        throwError (ErrorNoSuchFile [path] "don't have this file")
    else
      throwError (ErrorNoSuchPath [path] "don't have this folder")

-- | check errors and apply write if has file      
applyCommand (Write path txt) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
    throwError (ErrorWriteFile [path] "don't have this file")
  else
    if member (tail previousPath) newStateFileSystem then do
      let mapFolder = folderFilesIn (newStateFileSystem ! (tail previousPath))
      if member (head previousPath) mapFolder then
        writeCommand txt previousPath (mapFolder ! (head previousPath))
      else
        throwError (ErrorWriteFile [path] "don't have this file")
    else
      throwError (ErrorWriteFile [path] "don't have this file")

-- | check errors and apply cat if has path
applyCommand (Find path name) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
    recursiveFindCommand name (newStateFileSystem !previousPath) (addTrailingPathSeparator path)
  else
    throwError (ErrorFind [path] "don't have this folder")

-- | check errors and create file if has path
applyCommand (CreateFile path) = applyCreateCommand path (((void .) .) . createFileCommand)

-- | check errors and create file if has path
applyCommand (CreateFolder path) = applyCreateCommand path (((void .) .) . createFolderCommand)

-- | check errors and apply cd if has path
applyCommand (Cd path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
    cdCommand previousPath (newStateFileSystem ! previousPath)
  else
    throwError (ErrorFind [path] "don't have this folder")

-- | check errors and apply ls if has path
applyCommand (Ls path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
    lsCommand path
  else
    throwError (ErrorNoSuchPath [path] "don't have this folder")

-- | check errors and apply info if has path
applyCommand (Info path) = do
  newStatePath <- gets nowStatePath
  newStateFileSystem <- gets nowStateFileSystem
  let previousPath = (makePathFromTwo newStatePath (reverse (splitDirectories path)))
  if member previousPath newStateFileSystem then
      infoFolderCommand (newStateFileSystem ! previousPath)
  else
    if member (tail previousPath) newStateFileSystem then do
      let mapFolder = folderFilesIn (newStateFileSystem ! (tail previousPath))
      if member (head previousPath) mapFolder then
        infoFileCommand (mapFolder ! (head previousPath))
      else
        throwError (ErrorNoSuchFile [path] "don't have this file")
    else
      throwError (ErrorNoSuchPath [path] "don't have this folder")

-- | check errors and apply dir if has path
applyCommand (Dir) = dirCommand

-- | another commands skip
applyCommand other = return ()

-- | save all files which were made while working
saveFiles :: File -> IO()
saveFiles file = do
  isHasFolder <- doesDirectoryExist pathPath
  if (isHasFolder) then do
    putStrLn ("Error: already has such file name as file") 
  else do
    isHasFile <- doesFileExist pathPath
    if not isHasFile then do
      recSaveFiles
    else do
      permissionsFile <- getPermissions pathPath
      if writable permissionsFile then
        if not(executable permissionsFile) then
          recSaveFiles
        else
          putStr ("")
      else
        putStrLn ("Error: can't write in non-writable") 
          where
            pathPath = (joinPath (reverse (makePathFromTwo (filePath file) [fileName file])))
            recSaveFiles = do
              Data.ByteString.writeFile pathPath (fileContent file)
              setModificationTime pathPath (fileTime file)
              setPermissions pathPath (filePermissions file)
  
-- | save all folders which were made while working
saveFolders :: NowState -> [String] -> IO()
saveFolders state path = do
  isHasFolder <- doesDirectoryExist pathPath
  if (isHasFolder) then do
    permissionsFolder <- getPermissions pathPath
    if writable permissionsFolder then do
      recSaveFolders
    else 
      putStrLn ("Error: already has such file name") 
  else do
    isHasFile <- doesFileExist pathPath
    if not isHasFile then do
      recSaveFolders
    else 
      putStrLn ("Error: already has such file name")  
        where
          nameFolder = nowStateFileSystem state ! path
          pathPath = (joinPath (reverse path))
          recSaveFolders = do
            createDirectoryIfMissing True pathPath
            mapM_ (saveFolders state) (elems (folderFoldersIn nameFolder))
            mapM_ saveFiles (elems (folderFilesIn nameFolder))
  
-- | remove all folders and files which were removed while working
removeFoldersAndFiles :: NowState -> [String] -> IO()
removeFoldersAndFiles state path = do
  list <- listDirectory (joinPath (reverse path))
  let removed = filter filteredFunction (map (reverse . splitDirectories) list)
  mapM_ removeFunction removed
  mapM_ (removeFoldersAndFiles state) (elems (folderFoldersIn (nowStateFileSystem state ! path)))
 where
   nameFolders = map ((:[]) . head) (elems (folderFoldersIn (nowStateFileSystem state ! path)))
   nameFiles = map ((:[]) . fileName) (elems (folderFilesIn (nowStateFileSystem state ! path)))
   filteredFunction x = (notElem x nameFolders) && (notElem x nameFiles)
   removeFunction x = (removePathForcibly ((joinPath . reverse) (x ++ path)))

-- | save all changes (new and deleted file/folders and etc) which were made while working  
saveCommand :: NowState -> IO()
saveCommand state = do
 removeFoldersAndFiles state (nowStateRoot state)
 saveFolders state (nowStateRoot state)
