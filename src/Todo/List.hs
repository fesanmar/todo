module Todo.List 
    ( todoFilePath
    , extensionLen
    , nameFromPath
    , new
    , remove
    , rename
    , viewTodos
    ) where

import System.Directory (removeFile, renameFile, doesFileExist, listDirectory )
import System.IO ( hClose, hPutStr, openTempFile, openFile, IOMode (WriteMode) )
import System.FilePath
    ( joinPath, isExtensionOf, takeDirectory, takeFileName )
import qualified Data.Text as T

todoExtension :: String
todoExtension = ".todo"

extensionLen :: Int
extensionLen = length todoExtension

new :: FilePath -> IO ()
new fileName = do
    fileExist <- doesFileExist fileName
    if fileExist
    then alreadyExistsListError fileName
    else do
        handle <- openFile fileName WriteMode
        hClose handle

alreadyExistsListError :: FilePath -> IO ()
alreadyExistsListError fileName = putStrLn $ "Already exist a to-do list with the name " ++ "[" ++ fileName ++ "]"

remove :: FilePath  -> IO ()
remove = removeFile 

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

nameFromPath :: FilePath -> String 
nameFromPath fileName = T.unpack . T.dropEnd extensionLen $ T.pack $ takeFileName fileName

todoFilePath :: FilePath -> FilePath -> FilePath
todoFilePath todoDir fileName = joinPath [todoDir, fileName ++ todoExtension]