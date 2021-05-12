module Todo.FileHandling.Internal where

noSuchTodoList :: FilePath  -> String
noSuchTodoList fileName = "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"