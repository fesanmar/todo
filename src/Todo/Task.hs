module Todo.Task
  ( view
  , append
  , prepend
  , complete
  , bump
  , dropTask
  , mv
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
      getTodoItems',
      getItem,
      replaceFileContent,
      scroll,
      Direction(Down, Up) )
import Todo.Task.Internal
    ( emptyListMsg, emptyTaskMsg, outOfBoundErrorMsg, taskCompletedMsg, taskMovedMsg )

{-|
  Insert a task at the end of the file passed as an argument. If the file does
  not exist, it will return an error message wrapped in a 'Left'.
-}
append :: FilePath -> TodoTask -> IO (Either String ())
append = onValidTask append'

append' :: FilePath -> TodoTask -> IO ()
append' = appendFile

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
          return ()

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

{-|
  Bumps a task 'n' positions or up to the top of the list if 'n' is 'Nothing'.
-}
bump :: FilePath -> NumberString -> Maybe NumberString -> IO (Either String String)
bump fileName indexStr maybeSteps = onFileExistEither fileName $ do 
  scroll fileName Up indexStr maybeSteps
  return $ Right ""

{-|
  Drop a task 'n' positions or down to the bottom of the list if 'n' is 'Nothing'.
-}
dropTask :: FilePath -> NumberString -> Maybe NumberString -> IO (Either  String String)
dropTask fileName indexStr maybeSteps = onFileExistEither fileName $ do 
  scroll fileName Down indexStr maybeSteps
  return $ Right ""

{-|
  Moves a task from a list to another if both files exist and 'NumberString'
  contains a valid index.
-}
mv :: FilePath -> NumberString -> FilePath -> IO (Either  String String)
mv from indexStr to = do
  onFileExistEither from $ do 
    onFileExistEither to $ do
      items <- getTodoItems' from
      let mv' itm itms = append to itm
                          >> complete from indexStr 
                          >> return (taskMovedMsg itm to)
      onTaskExist indexStr items mv'