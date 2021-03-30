module Todo.FileHandling where

import System.Directory (doesFileExist)
import Control.Monad (when)

onFileExist :: FilePath -> IO () -> IO ()
onFileExist file io = do
    exist <- doesFileExist file
    when exist io