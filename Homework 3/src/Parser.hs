module Parser where

import Options.Applicative
import Command
import System.Directory.Internal.Prelude
import Control.Exception.Safe as Safe
import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Char8 as BS8 (ByteString, pack, unpack, putStrLn) 
import Data.Time (getCurrentTime)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runState)
import Control.Monad.Writer.Strict
import Data.Either (isLeft, fromLeft, lefts)

runParser :: String -> ParserResult Commands
runParser txt = execParserPure defaultPrefs (info (helper <*> allParser)
                                               (fullDesc <> header "File-manager" <> progDesc "my-best-file-manager"))
                                            (words txt)

parserParser :: String -> Parser Commands -> String -> Mod CommandFields Commands
parserParser name parser description = command name (info parser (progDesc description))

-- parser states
cdParser :: Mod CommandFields Commands
cdParser = parserParser "cd"
                        (Cd <$> strArgument (metavar "<path>" <> help "path to go"))
                        "Go to folder"
lsParser :: Mod CommandFields Commands
lsParser = parserParser "ls"
                        (Ls <$> strArgument (metavar "<path>" <> help "path to show"))
                        "Show files and folders in folder"

infParser :: Mod CommandFields Commands
infParser = parserParser "info"
                        (Info <$> strArgument (metavar "<path>" <> help "path to show info"))
                        "Show info of file or folder"

dirParser :: Mod CommandFields Commands
dirParser = parserParser "dir" (pure Dir) "Show files and folders in temprorary folder"

createFolderParser :: Mod CommandFields Commands
createFolderParser = parserParser "create-folder"
                        (CreateFolder <$> strArgument (metavar "<name>" <> help "name of new folder"))
                        "Create folder in folder"

createFileParser :: Mod CommandFields Commands
createFileParser = parserParser "create-file"
                        (CreateFile <$> strArgument (metavar "<name>" <> help "name of new file"))
                        "Create file in folder"

catParser :: Mod CommandFields Commands
catParser = parserParser "cat"
                        (Cat <$> strArgument (metavar "<name>" <> help "name of file"))
                        "Show file content"

removeFolderParser :: Mod CommandFields Commands
removeFolderParser = parserParser "remove-folder"
                        (RemoveFolder <$> strArgument (metavar "<name>" <> help "name of folder"))
                        "Remove folder in folder"

removeFileParser :: Mod CommandFields Commands
removeFileParser = parserParser "remove-file"
                        (RemoveFile <$> strArgument (metavar "<name>" <> help "name of file"))
                        "Remove file in folder"

isEmptyParser :: Mod CommandFields Commands
isEmptyParser = parserParser "is-empty"
                        (IsEmpty <$> strArgument (metavar "<path>" <> help "path file or folder"))
                        "Show file/folder is empty or not"
                        
writeParser :: Mod CommandFields Commands
writeParser = parserParser "write-file"
                           (Write <$> strArgument (metavar "<path>*" <> help "path of file") <*>
                           strArgument (metavar "<content>" <> help "new content")
                           )
                           "Write content into file"

findParser :: Mod CommandFields Commands
findParser = parserParser "find"
                           (Find <$> strArgument (metavar "<path>*" <> help "path, from where we start find") <*>
                           strArgument (metavar "<name>" <> help "name of file or folder")
                           )
                           "Find in folder and subfolders file/folder"

saveParser :: Mod CommandFields Commands
saveParser = parserParser "save" (pure Save) "Save changes"

exitParser :: Mod CommandFields Commands
exitParser = parserParser "exit" (pure Exit) "Save changes and break"
-- end parser states

-- | all types of states
allParser :: Parser Commands
allParser = hsubparser (cdParser <> createFolderParser <> createFileParser <> catParser <> removeFolderParser
                        <> removeFileParser <> writeParser <> findParser <> saveParser <> exitParser <> lsParser
                        <> dirParser <> infParser <> isEmptyParser)
 
-- | parse command and make action                           
runByParser :: NowState -> IO ()
runByParser state = do
  textCommand <- getLine
  command <- catchErrorParser (runParser textCommand)
  case command of
    Just Save -> do
      saveCommand state
      putStrLn ("Saved changes")
      hFlush stdout
      Safe.onException (runByParser state) (saveCommand state)
    Just Exit -> do
      saveCommand state
      putStrLn ("Saved changes and exit")
      hFlush stdout
    Just otherCommand -> do
      nowTime <- getCurrentTime
      let ((answer, result), newState) = runState (runWriterT (runReaderT (runExceptT $ applyCommand otherCommand) 
                                                                           nowTime)) state
      if isLeft answer then do
        print (head (lefts [answer]))
      else do
         putStrLn ("Done!")
         BS8.putStrLn result
         hFlush stdout
      Safe.onException (runByParser newState) (saveCommand newState)
    Nothing -> do
      putStrLn ("")
      hFlush stdout
      Safe.onException (runByParser state) (saveCommand state)
  where 
    catchErrorParser :: ParserResult a -> IO (Maybe a)
    catchErrorParser (Failure fail) = do
      let (message, e) = renderFailure fail ""
      putStrLn message
      return Nothing
    catchErrorParser suc = Just <$> handleParseResult suc 
            