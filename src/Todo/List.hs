module Todo.List
    ( new
    , remove
    , rename
    , viewTodos
    , notTodoListToShowMsg
    ) where

import System.Directory
    ( doesFileExist, listDirectory, removeFile, renameFile ) 
import System.IO ( hClose, openFile, IOMode(WriteMode) ) 
import System.FilePath ( joinPath, isExtensionOf, takeDirectory )
import Control.Monad ( when ) 
import Todo.List.Internal ( alreadyExistsListError )
import Todo.FileHandling
    ( onFileExist, nameFromPath, todoExtension )

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
remove :: FilePath  -> IO (Either String ())
remove file = onFileExist file $ removeFile file

{-|
  Renames a to-do list if exist and if the other name doesn't belong
  to another existing to-do list.
  If the list to rename doesn't exist, 'rename' does nothing. If
  the wanted new name belongs to another list, 'rename' prints an
  error message.
-}
rename :: FilePath -> String -> IO (Either String ())
rename todoFile newName= onFileExist todoFile $ do
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