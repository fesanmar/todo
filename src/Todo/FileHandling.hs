module Todo.FileHandling where

import System.Directory (doesFileExist)
import App.Messages (noSuchTodoList)

onFileExist :: FilePath -> IO () -> IO (Either String ())
onFileExist file io =
  doesFileExist file
    >>= \exist ->
      if exist
        then io >> return (Right ())
        else return . Left $ noSuchTodoList file