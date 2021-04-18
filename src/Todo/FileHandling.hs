module Todo.FileHandling where

import qualified Data.Text as T
import System.Directory ( doesFileExist )
import App.Messages ( noSuchTodoList )
import System.FilePath ( takeFileName )

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