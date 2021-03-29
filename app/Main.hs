module Main where
  
import System.Directory ( getAppUserDataDirectory )
import Config ( loadConfig )
import System.Directory.Internal.Prelude ( getArgs )
import Command.Dispatcher ( dispatch )

todoDirName :: String
todoDirName = ".todo"

main :: IO ()
main = do
  todoDir <- getAppUserDataDirectory todoDirName
  config <- loadConfig todoDir
  arguments <- getArgs
  dispatch config arguments