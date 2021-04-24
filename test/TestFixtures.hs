module TestFixtures where

import App.Config ( loadConfig, Config )
import System.FilePath ( joinPath )
import System.Directory ( removePathForcibly )

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