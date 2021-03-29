module Main where

import Command.Distapcher ( dispatch )
import System.Directory ( getAppUserDataDirectory )
import Config ( loadConfig )
import System.Directory.Internal.Prelude ( getArgs )

todoDirName :: String
todoDirName = ".todo"

main :: IO ()
main = do
  todoDir <- getAppUserDataDirectory todoDirName
  config <- loadConfig todoDir
  arguments <- getArgs
  dispatch config arguments