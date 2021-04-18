module App.Messages where

noSuchTodoList :: String -> String
noSuchTodoList fileName = "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"

emptyTaskMsg :: String
emptyTaskMsg = "Can't insert empty task."