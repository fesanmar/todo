module Todo.FileHandling ( onFileExist
                         , onFileExistEither
                         , nameFromPath
                         , todoExtension
                         , listExistsOnDir
                         ) where

import Todo.FileHandling.Internal ( noSuchTodoList )
import qualified Data.Text as T
import Data.Either
import System.Directory ( doesFileExist )
import System.FilePath ( takeFileName, joinPath )

todoExtension :: String
todoExtension = ".todo"

extensionLen :: Int
extensionLen = length todoExtension

nameFromPath :: FilePath -> String
nameFromPath fileName = T.unpack . T.dropEnd extensionLen $ T.pack $ takeFileName fileName

onFileExist :: FilePath -> IO a -> IO (Either String a)
onFileExist file io =
  doesFileExist file
    >>= \exist ->
      if exist
        then io >>= \x -> return (Right x)
        else return . Left . noSuchTodoList $ nameFromPath file

onFileExistEither :: FilePath -> IO (Either String r) -> IO (Either String r)
onFileExistEither file io = do 
  doesFileExist file
    >>= \exist ->
    if exist
        then io >>= \x -> return x
        else return . Left . noSuchTodoList $ nameFromPath file

listExistsOnDir :: FilePath -> FilePath -> IO (FilePath, Bool)
listExistsOnDir todoDir "" = return ("", False)
listExistsOnDir todoDir fileName =
  let todoFile = todoFilePath todoDir fileName
   in doesFileExist todoFile
      >>= \fileExists -> return (todoFile, fileExists)

todoFilePath :: FilePath -> FilePath -> FilePath
todoFilePath todoDir fileName = joinPath [todoDir, fileName ++ todoExtension]