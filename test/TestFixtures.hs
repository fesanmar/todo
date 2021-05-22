module TestFixtures where

import Data.Char
import App.Config ( loadConfig, Config )
import System.FilePath ( joinPath )
import System.Directory ( removePathForcibly )
import Todo.Task.Internal ( outOfBoundErrorMsg, taskMovedMsg )
import Util.Console ( putErrorLn )
import Todo.Transaction ( TodoTask )

baseDirName :: FilePath
baseDirName = joinPath [".", "test", "res"]

basePath :: FilePath
basePath = joinPath [baseDirName, ".todo"]

iniFilePath :: FilePath
iniFilePath = joinPath [basePath, "todo.ini"]

cleanUpDir :: IO ()
cleanUpDir = removePathForcibly basePath

loadTestConfig :: IO Config
loadTestConfig = loadConfig basePath

configAndCleanUpDir :: IO Config
configAndCleanUpDir = do
    config <- loadConfig basePath
    cleanUpDir
    return config

outOfBoundError :: String -> IO ()
outOfBoundError = putErrorLn . outOfBoundErrorMsg

taskMoved :: TodoTask -> FilePath  -> IO ()
taskMoved t = putStrLn . taskMovedMsg t

cleanOutput :: String -> String
cleanOutput = filter (/= '\r')