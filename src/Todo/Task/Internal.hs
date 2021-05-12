module Todo.Task.Internal where

import Todo.FileHandling ( nameFromPath )
import Todo.Transaction ( NumberString, TodoTask )

emptyListMsg :: FilePath -> String
emptyListMsg fileName = nameFromPath fileName ++ " is empty."

emptyTaskMsg :: String
emptyTaskMsg = "Can't insert empty task."

outOfBoundErrorMsg :: NumberString -> String
outOfBoundErrorMsg numberString = show numberString ++ " is out of bound or is not a number!"

taskCompletedMsg :: TodoTask -> String
taskCompletedMsg = (++) "Completed: "