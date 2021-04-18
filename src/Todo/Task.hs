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
import Data.Either
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
import App.Messages ( emptyTaskMsg )

append :: FilePath -> String -> IO (Either String ())
append = onValidTask appendFile

onValidTask :: (FilePath -> String -> IO ()) -> FilePath -> String -> IO (Either String ())
onValidTask func file t 
  | T.null . T.strip . T.pack $ t = return $ Left emptyTaskMsg
  | otherwise = onFileExist file . func file $ t ++ "\n"

prepend :: FilePath -> String -> IO ()
prepend fileName todoItem =
  append fileName todoItem
    >> lastIndex fileName
    >>= \index -> bump fileName (show index) Nothing

view :: String -> IO ()
view fileName = do
  todoTasks <- getTodoItems fileName
  let isEmpty = null todoTasks || all (T.null . T.strip . T.pack) todoTasks
  if isEmpty
    then putStrLn $ nameFromPath fileName ++ " is empty."
    else printListedTask todoTasks

printListedTask :: [TodoTask] -> IO ()
printListedTask todoTasks =
  let numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [0 ..] todoTasks
   in mapM_ putStrLn numberedTasks

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