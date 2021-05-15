module Folder where
import System.Directory (Permissions)
import Data.Time.Clock (UTCTime)
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map)
import File

-- | data with all information about folder
data Folder = Folder {
    folderName        :: String
  , folderPath        :: [String]
  , folderPermissions :: Permissions
  , folderTime        :: UTCTime
  , folderFilesIn     :: Map String File
  , folderFoldersIn   :: Map String [String]
  , folderSize        :: Integer
} deriving (Eq, Show)
