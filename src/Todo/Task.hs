module Todo.Task
  ( view
  , append
  , prepend
  , complete
  , bump
  , dropTask
  )
where

import qualified Data.List as L
import Data.Either ()
import Data.Maybe ( fromJust, isJust )
import qualified Data.Text as T
import Todo.FileHandling ( nameFromPath, onFileExist )
import Todo.Transaction
    ( NumberString,
      TodoTask,
      outOfBoundError,
      lastIndex,
      getTodoItems,
      getItem,
      replaceFileContent,
      scroll,
      Direction(Down, Up) )
import Todo.Task.Internal ( emptyListMsg, emptyTaskMsg )

append :: FilePath -> String -> IO (Either String ())
append = onValidTask appendFile

prepend :: FilePath -> String -> IO (Either String ())
prepend = onValidTask appendAndBump
  where appendAndBump fileName todoItem = do
          appendFile fileName todoItem 
          index <- lastIndex fileName 
          bump fileName (show index) Nothing

onValidTask :: (FilePath -> String -> IO ()) -> FilePath -> String -> IO (Either String ())
onValidTask func file t 
  | T.null . T.strip . T.pack $ t = return $ Left emptyTaskMsg
  | otherwise = onFileExist file . func file $ t ++ "\n"

{-|
  Returns an error message if the file passed as an argument does not exist.
  Otherwise, it returns a numbered task chain or an empty list message if 
  the file contains no tasks.
-}
view :: FilePath -> IO (Either String String)
view fileName = onFileExist fileName $ do
  todoTasks <- getTodoItems fileName
  let isEmpty = null todoTasks || all (T.null . T.strip . T.pack) todoTasks
  if isEmpty
    then return $ emptyListMsg fileName
    else return $ numberedTask todoTasks

numberedTask :: [TodoTask] -> String
numberedTask = unlines . zipWith (\n task -> show n ++ " - " ++ task) [0 ..]

complete :: [String] -> IO ()
complete [fileName, numberString] = do
  items <- getTodoItems fileName
  let itemToDelete = getItem numberString items
  if isJust itemToDelete
    then replaceFileContent fileName (unlines $ L.delete (fromJust itemToDelete) items)
    else outOfBoundError numberString

bump :: FilePath -> NumberString -> Maybe NumberString -> IO ()
bump fileName = scroll fileName Up

dropTask :: FilePath -> NumberString -> Maybe NumberString -> IO ()
dropTask fileName = scroll fileName Down