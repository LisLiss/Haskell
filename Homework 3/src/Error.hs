module Error where
import System.Directory (Permissions)
import Data.Time.Clock (UTCTime)
import Data.ByteString.Char8 (ByteString)
import System.FilePath (joinPath)

-- | data with all errors in file-manager
data Errors =
    ErrorNoSuchFile [String] String
  | ErrorNoSuchPath [String] String
  | ErrorRemoveFile [String] String
  | ErrorRemoveFolder [String] String
  | ErrorWriteFile [String] String
  | ErrorCreate [String] String
  | ErrorCatFile [String] String
  | ErrorFind [String] String

instance Show Errors where
  show (ErrorNoSuchFile path message) = "Error NO SUCH FILE:" ++ (joinPath path) ++ " " ++ message
  show (ErrorNoSuchPath path message) = "Error NO SUCH PATH: " ++ (joinPath path) ++ " " ++ message
  show (ErrorRemoveFile path message) = "Error REMOVE FILE: " ++ (joinPath path) ++ " " ++ message
  show (ErrorRemoveFolder path message) = "Error REMOVE FOLDER: " ++ (joinPath path) ++ " " ++ message
  show (ErrorWriteFile path message) = "Error WRITE FILE: " ++ (joinPath path) ++ " " ++ message
  show (ErrorCreate path message) = "Error CREATE:" ++ (joinPath path) ++ " " ++ message
  show (ErrorCatFile path message) = "Error CAT: " ++ (joinPath path) ++ " " ++ message
  show (ErrorFind path message) = "Error FIND: " ++ (joinPath path) ++ " " ++ message

