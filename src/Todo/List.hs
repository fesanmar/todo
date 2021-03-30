module Todo.List 
    ( new
    , remove
    , rename
    , viewTodos
    , nameFromPath
    , listExistsOnDir
    , notTodoListToShowMsg
    ) where

import System.Directory ( removeFile, renameFile, doesFileExist, listDirectory )
import System.IO ( hClose, hPutStr, openTempFile, openFile, IOMode (WriteMode) )
import System.FilePath
    ( joinPath, isExtensionOf, takeDirectory, takeFileName )
import qualified Data.Text as T
import Control.Monad ( when )
import Util.Console ( putErrorLn )
import Control.Monad ( when )
import Todo.List.Internal (alreadyExistsListError)

todoExtension :: String
todoExtension = ".todo"

extensionLen :: Int
extensionLen = length todoExtension

{-|
  Creates a new to-do list if not exist. Otherwise, 
  displays an error message.
-}
new :: FilePath -> IO ()
new fileName = do
    fileExist <- doesFileExist fileName
    if fileExist
    then alreadyExistsListError $ nameFromPath fileName
    else do
        handle <- openFile fileName WriteMode
        hClose handle

{-|
  Removes a to-do list if exist. Otherwise, 'remove'
  does nothing.
-}
remove :: FilePath  -> IO ()
remove file = do
    exist <- doesFileExist file
    when exist $ removeFile file

rename :: FilePath -> FilePath -> IO ()
rename todoFile newName= do
    let todoPath = takeDirectory todoFile
    let newTodoFile  = joinPath [todoPath, newName ++ todoExtension]
    doesExists <- doesFileExist newTodoFile
    if doesExists
    then alreadyExistsListError newName
    else renameFile todoFile newTodoFile

viewTodos :: FilePath -> IO ()
viewTodos todoDir = do
    dirContent <- listDirectory todoDir
    let todos = map nameFromPath . filter (todoExtension `isExtensionOf`) $ dirContent
    mapM_ putStrLn todos
    when (null todos) notTodoListToShowMsg

notTodoListToShowMsg :: IO ()
notTodoListToShowMsg = putStrLn "Not to-do list to show yet. Create one using <new> command."

listExistsOnDir :: FilePath -> FilePath -> IO (FilePath, Bool)
listExistsOnDir todoDir "" = return ("", False)
listExistsOnDir todoDir fileName =
  let todoFile = todoFilePath todoDir fileName
   in doesFileExist todoFile
      >>= \fileExists -> return (todoFile, fileExists)

nameFromPath :: FilePath -> String 
nameFromPath fileName = T.unpack . T.dropEnd extensionLen $ T.pack $ takeFileName fileName

todoFilePath :: FilePath -> FilePath -> FilePath
todoFilePath todoDir fileName = joinPath [todoDir, fileName ++ todoExtension]