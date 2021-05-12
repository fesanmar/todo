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
import Data.Functor ( (<&>) )
import qualified Data.Text as T
import Todo.FileHandling ( onFileExist, onFileExistEither ) 
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
import Todo.Task.Internal
    ( emptyListMsg, emptyTaskMsg, outOfBoundErrorMsg, taskCompletedMsg )

{-|
  Insert a task at the end of the file passed as an argument. If the file does
  not exist, it will return an error message wrapped in a 'Left'.
-}
append :: FilePath -> TodoTask -> IO (Either String ())
append = onValidTask appendFile

{-|
  Insert a task at the beginning of the file passed as an argument. If the file
  does not exist, it will return an error message wrapped in a 'Left'.
-}
prepend :: FilePath -> TodoTask -> IO (Either String ())
prepend = onValidTask appendAndBump
  where appendAndBump fileName todoItem = do
          appendFile fileName todoItem 
          index <- lastIndex fileName 
          bump fileName (show index) Nothing

onValidTask :: (FilePath -> TodoTask -> IO ()) -> FilePath -> TodoTask -> IO (Either String ())
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

{-|
  Deletes the task in the 'NumberString' position from the 'FilePath'
  to-do list. Yields a successful message if the file exists an the 
  position is not out of bound. Otherwise, yields the accurate error
  message.
-}
complete :: FilePath -> NumberString -> IO (Either String String)
complete fileName numberString = onFileExistEither fileName $ do
  items <- getTodoItems fileName
  let removeTaskInFile task tasksLst =
        replaceFileContent fileName (unlines $ L.delete task tasksLst)
          >> return (taskCompletedMsg task)
  onTaskExist numberString items removeTaskInFile

onTaskExist :: Show a => NumberString -> [TodoTask] -> (TodoTask -> [TodoTask] -> IO a) -> IO (Either String a)
onTaskExist numberString items io
  | isJust task = io (fromJust task) items <&> Right
  | otherwise = return $ Left $ outOfBoundErrorMsg numberString
  where task = getItem numberString items

bump :: FilePath -> NumberString -> Maybe NumberString -> IO ()
bump fileName = scroll fileName Up

dropTask :: FilePath -> NumberString -> Maybe NumberString -> IO ()
dropTask fileName = scroll fileName Down