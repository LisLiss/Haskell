module Main where

import Lib
import Command
import Error
import File
import Folder
import Parser
import Manager
import Control.Exception.Safe as Safe
import System.Directory
import System.Directory.Internal.Prelude

main :: IO ()
main = do
  root <- rootInitialize
  manager <- managerInitialize root
  putStrLn "my-best-file-manager"
  putStrLn "You can use --help"
  hFlush stdout
  Safe.onException (runByParser manager) (saveCommand manager)



