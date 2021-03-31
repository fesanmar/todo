{-# LANGUAGE OverloadedStrings #-}
module App.Config 
    ( loadConfig
    , configToList
    , Config (path, configFilePath, defaultList)
    , dumpConfig
    , newDefaultList ) where

import System.FilePath ( joinPath )
import Data.Maybe ( fromJust, isJust )
import qualified Data.Text as T
import qualified Data.ByteString as Str
import qualified Data.ByteString.UTF8 as BUT
import App.Config.Internal
    ( createTodoDirIfMissing, iniContent, extractKVPairs )

-- |Holds the app configuration data.
data Config = Config { path :: FilePath
                     , configFilePath :: FilePath
                     , defaultList :: Maybe String
                     } deriving ( Eq )

instance Show Config where
  show config = if isJust defaultTodo then "defaultList=" ++ fromJust defaultTodo ++ "\n" else ""
    where defaultTodo = defaultList config

configFilename :: FilePath
configFilename = "todo.ini"

{-|
  Loads default configuration if 'todo.ini' doesn't exists yet or its empty.
  Otherwise, the todo.ini file is loaded as 'Config' and retured. 
-}
loadConfig :: FilePath -> IO Config
loadConfig basePath = do
    appPath <- createTodoDirIfMissing basePath
    let iniFilePath = joinPath [appPath, configFilename]
    ini <- iniContent iniFilePath
    let configPairs = extractKVPairs ini
        defaultLst = lookup "defaultList" configPairs
    return $ Config appPath iniFilePath defaultLst 

-- |Returns the configuration in the form of a list of strings with the format key=value
configToList :: Config -> [String]
configToList = map T.unpack . T.lines . T.strip . T.pack . show

-- |Dumps 'Config' into 'todo.ini' file.
dumpConfig :: Config -> IO ()
dumpConfig config = Str.writeFile (configFilePath config) (BUT.fromString $ show config)

-- |Returns a 'Config' with a new to-do list as default.
newDefaultList :: String -> Config -> Config
newDefaultList newTodo (Config pth cf _)  = Config pth cf $ Just newTodo