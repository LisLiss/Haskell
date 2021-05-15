module File where
import System.Directory (Permissions)
import Data.Time.Clock (UTCTime)
import Data.ByteString.Char8 (ByteString)

-- | data with all information about file
data File = File {
    fileName        :: String
  , filePath        :: [String]
  , filePermissions :: Permissions
  , fileTime        :: UTCTime
  , fileContent     :: ByteString
  , fileSize        :: Integer
} deriving (Eq, Show)

