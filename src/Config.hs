{-# LANGUAGE OverloadedStrings #-}
module Config 
    ( loadConfig
    , Config (path, configFilePath, defaultList)
    , dumpConfig
    , newDefaultList ) where

import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import System.FilePath ( joinPath )
import Data.Maybe ( fromJust, isJust )
import Data.Tuple.Extra ( both, second )
import qualified Data.Text as T
import qualified Data.ByteString as Str
import qualified Data.ByteString.UTF8 as BUT

data Config = Config { path :: FilePath
                     , configFilePath :: FilePath
                     , defaultList :: Maybe FilePath
                     } deriving ( Eq )

instance Show Config where
  show config = if isJust defaultTodo then "defaultList=" ++ fromJust defaultTodo ++ "\n" else ""
    where defaultTodo = defaultList config

todoDirName :: FilePath
todoDirName = ".todo"

configFilename :: FilePath
configFilename = "todo.ini"

loadConfig :: FilePath -> IO Config
loadConfig basePath = do
    appPath <- createTodoDirIfMissing basePath
    let iniFilePath = joinPath [appPath, configFilename]
    configContentMaybe <- iniContent iniFilePath
    config <- iniContent iniFilePath
    let configPairs = extractKVPairs config
        defaultLst = lookup "defaultList" configPairs
    return $ Config appPath iniFilePath defaultLst 

createTodoDirIfMissing :: FilePath -> IO FilePath
createTodoDirIfMissing basePath = let todoDir = joinPath [basePath, todoDirName] in
  doesDirectoryExist todoDir
  >>= \existsDir -> createDirectoryIfMissing existsDir todoDir
  >> return todoDir

iniContent :: FilePath -> IO Str.ByteString
iniContent iniFile =
  doesFileExist iniFile
    >>= \exists ->
      if exists
        then Str.readFile iniFile >>= \content -> return content
        else return ""

extractKVPairs :: Str.ByteString -> [(String, String)]
extractKVPairs = createPairs . removeNoKeyValuePairs . removeSections . removeComments . textLines . BUT.toString
  where textLines = map T.pack . lines
        removeComments = filter (not . T.isPrefixOf ";")
        removeSections = filter (not . T.isPrefixOf "[")
        removeNoKeyValuePairs = filter ("=" `T.isInfixOf`)
        createPairs = map (both (T.unpack . T.strip) . second (T.drop 1) . T.break (== '='))

dumpConfig :: Config -> IO ()
dumpConfig config = Str.writeFile (configFilePath config) (BUT.fromString $ show config)

newDefaultList :: FilePath -> Config -> Config
newDefaultList newTodo (Config pth cf _)  = Config pth cf $ Just newTodo