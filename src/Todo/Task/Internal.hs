module Todo.Task.Internal where

import Todo.FileHandling ( nameFromPath )

emptyListMsg :: FilePath -> String
emptyListMsg fileName = nameFromPath fileName ++ " is empty."

emptyTaskMsg :: String
emptyTaskMsg = "Can't insert empty task."