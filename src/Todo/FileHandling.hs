module Todo.FileHandling (
                         onFileExist
                         , nameFromPath
                         , todoExtension
                         , listExistsOnDir
                         ) where

import qualified Data.Text as T
import System.Directory ( doesFileExist )
import App.Messages ( noSuchTodoList )
import System.FilePath ( takeFileName, joinPath )

todoExtension :: String
todoExtension = ".todo"

extensionLen :: Int
extensionLen = length todoExtension

nameFromPath :: FilePath -> String
nameFromPath fileName = T.unpack . T.dropEnd extensionLen $ T.pack $ takeFileName fileName

onFileExist :: FilePath -> IO () -> IO (Either String ())
onFileExist file io =
  doesFileExist file
    >>= \exist ->
      if exist
        then io >> return (Right ())
        else return . Left $ noSuchTodoList file

listExistsOnDir :: FilePath -> FilePath -> IO (FilePath, Bool)
listExistsOnDir todoDir "" = return ("", False)
listExistsOnDir todoDir fileName =
  let todoFile = todoFilePath todoDir fileName
   in doesFileExist todoFile
      >>= \fileExists -> return (todoFile, fileExists)

todoFilePath :: FilePath -> FilePath -> FilePath
todoFilePath todoDir fileName = joinPath [todoDir, fileName ++ todoExtension]