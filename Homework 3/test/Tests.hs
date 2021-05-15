{-# LANGUAGE OverloadedStrings #-}

module Tests where

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (getCurrentDirectory)
import System.FilePath (combine, splitDirectories, dropFileName)
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Lazy (runState)
import Control.Monad.Except (runExceptT)
import Data.Map.Strict ((!), (!?), member, notMember)
import Data.Time.Clock.POSIX (getCurrentTime)
import Control.Monad (void)
import Data.Either (isLeft, fromLeft, lefts, isRight)
import Command
import Error
import File
import Folder
import Parser
import Manager

createState :: IO NowState
createState = do
  root <- getCurrentDirectory
  let path = combine root "testingFolder"
  managerInitialize path

-- checker folders functions
checkFolder ::FilePath -> NowState -> Bool
checkFolder path state = member (makePathFromTwo (nowStateRoot state) ((reverse . splitDirectories) path))
                                (nowStateFileSystem state)

notHaveFolder :: FilePath -> NowState -> Assertion
notHaveFolder path state =
  if not (checkFolder path state) then
    return ()
  else
    assertFailure ("Not expected folder " ++ path)

haveFolder :: FilePath -> NowState -> Assertion
haveFolder path state =
  if (checkFolder path state) then
    return ()
  else
    assertFailure ("Expected folder " ++ path)
-- end of checker folders functions

-- checker files functions
checkFile ::FilePath -> NowState -> Bool
checkFile path state = do
  let fullPath = (makePathFromTwo (nowStateRoot state) ((reverse . splitDirectories) path))
  if member (tail fullPath) (nowStateFileSystem state) then
    member (head fullPath) (folderFilesIn ((nowStateFileSystem state) ! (tail fullPath)))
  else
    False

notHaveFile :: FilePath -> NowState -> Assertion
notHaveFile path state =
  if not (checkFile path state) then
    return ()
  else
    assertFailure ("Not expected file " ++ path)

haveFile :: FilePath -> NowState -> Assertion
haveFile path state =
  if (checkFile path state) then
    return ()
  else
    assertFailure ("Expected file " ++ path)
-- end checker files functions

-- | check if function made smth
checkerReturnStateGood state command wanted function = do
  nowTime <- getCurrentTime
  let ((res, real), newState) = runState (runWriterT (runReaderT (runExceptT $ applyCommand command) nowTime)) state
  if isLeft res then
    assertFailure (show (lefts [res]))
  else do
    real @?= wanted
    function newState
    return newState
    
-- | check if function didn't made smth
checkerReturnStateFail state command wanted function = do
  nowTime <- getCurrentTime
  let ((res, real), newState) = runState (runWriterT (runReaderT (runExceptT $ applyCommand command) nowTime)) state
  if isRight res then
    assertFailure ("Go to non-expected folder")
  else do
    return newState

checker = (((void .) .) .) . checkerReturnStateGood
checkerFail = (((void .) .) .) . checkerReturnStateFail

-- | manually go to path
goToFolder state path = do 
  let newPath = makePathFromTwo (nowStatePath state) 
                                ((reverse . splitDirectories) path)
  state {nowStatePath = newPath, 
         nowStateFolder = nowStateFileSystem state ! newPath}

testingCd :: Assertion
testingCd = do
  state <- createState
  newState <- checkerReturnStateGood state (Cd "1") "" (@?= goToFolder state "1")
  newState2 <- checkerReturnStateGood newState (Cd "fff") "" (@?= goToFolder state "1/fff")
  newState3 <- checkerReturnStateGood newState2 (Cd "..") "" (@?= goToFolder state "1")
  newState4 <- checkerReturnStateGood newState3 (Cd "..") "" (@?= state)
  newState5 <- checkerReturnStateGood newState4 (Cd "1/fff") "" (@?= goToFolder state "1/fff")
  newState6 <- checkerReturnStateGood newState5 (Cd "../..") "" (@?= state)
  newState7 <- checkerReturnStateFail newState6 (Cd "2") "" (@?= state)
  checker newState7 (Cd ".") "" (@?= newState7)

testingCreateFile :: Assertion
testingCreateFile = do
  state <- createState
  checker state (CreateFile "r") "" (haveFile "r")
  checker state (CreateFile "r.txt") "" (haveFile "r.txt")
  checker state (CreateFile "r.f") "" (haveFile "r.f")
  checker state (CreateFile "1r.jpg") "" (haveFile "1r.jpg")
  checker state (CreateFile "1/r") "" (haveFile "1/r")
  checker state (CreateFile "1/k.txt") "" (haveFile "1/k.txt")

testingCreateFolder :: Assertion
testingCreateFolder = do
  state <- createState
  checker state (CreateFolder "r") "" (haveFolder "r")
  checker state (CreateFolder "1/2") "" (haveFolder "1/2")
  checker state (CreateFolder "1/fff/3") "" (haveFolder "1/fff/3")
  checker state (CreateFolder "1/../4") "" (haveFolder "4")

testingWrite :: Assertion
testingWrite = do
  state <- createState
  newState <- checkerReturnStateGood state (Write "file.txt" "mau") "" (const $ return ())
  checker newState (Cat "file.txt") "mau" (@?= newState)
  newState2 <- checkerReturnStateGood newState (Write "file2" "gav") "" (const $ return ())
  newState3 <- checkerReturnStateGood newState2 (Write "file2" "kva") "" (const $ return ())
  newState4 <- checkerReturnStateGood newState3 (Write "file2" "muu") "" (const $ return ())
  checker newState4 (Cat "file2") "muu" (@?= newState4)

testingCat :: Assertion
testingCat = do
  state <- createState
  checker state (Cat "file.txt") "cat" (@?= state)
  checker state (Cat "file2") "rrr" (@?= state)

testingRemoveFile :: Assertion
testingRemoveFile = do
  state <- createState
  checker state (RemoveFile "file.txt") "" (notHaveFile "file.txt")
  checker state (RemoveFile "file2") "" (notHaveFile "file2")
  checker state (RemoveFile "cat.txt") "" (notHaveFile "cat.txt")
  checker state (RemoveFile "dog.txt") "" (notHaveFile "dog.txt")
  checker state (RemoveFile "1/fff/5") "" (notHaveFile "1/fff/5")

testingRemoveFolder :: Assertion
testingRemoveFolder = do
  stateT <- createState
  stateT2 <- checkerReturnStateGood stateT (CreateFolder "a") "" (haveFolder "a")
  stateT3 <- checkerReturnStateGood stateT2 (CreateFolder "a/d") "" (haveFolder "a/d")
  stateT4 <- checkerReturnStateGood stateT3 (CreateFolder "b") "" (haveFolder "b")
  state <- checkerReturnStateGood stateT4 (CreateFolder "c") "" (haveFolder "c")
  checker state (RemoveFolder "a/d") "" (notHaveFolder "a/d")
  checker state (RemoveFolder "b") "" (notHaveFolder "b")
  checker state (RemoveFolder "c") "" (notHaveFolder "c")

testingFind :: Assertion
testingFind = do
  stateT <- createState
  stateT2 <- checkerReturnStateGood stateT (CreateFolder "a") "" (haveFolder "a")
  stateT3 <- checkerReturnStateGood stateT2 (CreateFolder "a/d") "" (haveFolder "a/d")
  stateT4 <- checkerReturnStateGood stateT3 (CreateFolder "b") "" (haveFolder "b")
  state <- checkerReturnStateGood stateT4 (CreateFolder "c") "" (haveFolder "c")
  checker state (Find "." "file.txt") ".\\file.txt\n" (@?= state)
  checker state (Find "a" "d") "a\\d\n" (@?= state)


myTests :: TestTree
myTests =
   testGroup
     "Testing for my file manager"
     [testCase "Cd" testingCd
     , testCase "Create file" testingCreateFile
     , testCase "Create folder" testingCreateFolder
     , testCase "Write" testingWrite
     , testCase "Cat" testingCat
     , testCase "Remove file" testingRemoveFile
     , testCase "Remove folder" testingRemoveFolder
     , testCase "Find" testingFind
     ]