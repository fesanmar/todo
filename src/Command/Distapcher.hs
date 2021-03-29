module Command.Distapcher ( dispatch ) where

import Config
    ( newDefaultList,
      dumpConfig,
      configToList,
      Config(defaultList, path) )
import Control.Monad (when)
import Todo.List
    ( new, remove, rename, viewTodos, listExistsOnDir )
import Util.Console (putErrorLn)
import Todo.Task
    ( append, prepend, view, complete, bump, dropTask )

type Command = String

dispatch :: Config -> [String] -> IO ()
dispatch config [] = usage
dispatch config ["help"] = usage
dispatch config ["ls"] = viewTodos (path config)
dispatch config ["config"] =
  let configLst = configToList config
   in mapM_ putStrLn configLst
        >> when (null configLst) (putStrLn "Any configuration setted yet.")
dispatch config ["dl", fileName] = do
  (_, fileExists) <- listExistsOnDir (path config) fileName
  if fileExists
    then dumpConfig $ newDefaultList fileName config
    else putErrorLn $ fileName ++ " doesn't exists. You should creat it firs using <new> command"
dispatch config (command : "-b" : fileName : args) = dispatch config (command : fileName : "-b" : args)
dispatch config (command : fileName : args) = do
  let file = accurateTodoFile fileName (defaultList config)
  (todoFile, fileExists) <- listExistsOnDir (path config) file
  if fileExists || not fileExists && command == "new"
    then runTodoCommand command $ todoFile : args
    else accurateTodoFileError fileName
dispatch todoDir [command] = notExistingCommandError command

runTodoCommand :: Command -> [String] -> IO ()
runTodoCommand "new" [fileName] = new fileName
runTodoCommand "remove" [fileName] = remove fileName
runTodoCommand "rename" [fileName, newFileName] = rename fileName newFileName
runTodoCommand "view" [fileName] = view fileName
runTodoCommand "add" [fileName, "-b", todoItem] = prepend fileName todoItem
runTodoCommand "add" [fileName, todoItem] = append fileName todoItem
runTodoCommand "complete" args@[fileName, numberString] = complete args
runTodoCommand "bump" [fileName, numberString] = bump fileName numberString Nothing
runTodoCommand "bump" [fileName, numberString, stepsStr] = bump fileName numberString (Just stepsStr)
runTodoCommand "drop" [fileName, numberString] = dropTask fileName numberString Nothing
runTodoCommand "drop" [fileName, numberString, stepsStr] = dropTask fileName numberString (Just stepsStr)
runTodoCommand command _ = notExistingCommandError command

accurateTodoFile :: String -> Maybe String -> String
accurateTodoFile "--" (Just file) = file
accurateTodoFile "--" Nothing = ""
accurateTodoFile file _ = file

accurateTodoFileError :: String -> IO ()
accurateTodoFileError "--" = errorAndUsage "There is no to-do list setted as default. Use <dl> to accomplish it." 
accurateTodoFileError fileName = putErrorLn $ "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"

notExistingCommandError :: String -> IO ()
notExistingCommandError command = errorAndUsage $ "There is no " ++ "<" ++ command ++ "> command or it's arguments doesn't match.\nPlease check usage:\n"

errorAndUsage :: String -> IO ()
errorAndUsage msg = putErrorLn msg >> usage

usage :: IO ()
usage =
  mapM_
    putStrLn
    [ "usage: todo <command> [<args>]\n",
      "These are the todo commands:",
      "    config                                Shows user's configuration",
      "    ls                                    Shows user's to-do lists",
      "    new <listName>                        Creates a new to-do list",
      "    remove <listName>                     removes a existing to-do list",
      "    rename <listName> <newName>           renames a existing to-do list",
      "    dl <listName>                         Sets <listName> as the default list, so '--' can be used instead of the list name.",
      "    view <listName>                       Shows a to-do list's tasks",
      "    add [-b] <listName> <task>            append a new task to the passed to-do list or prepend it if [-b] is setted",
      "    complete <listName> <taskIndex>       complete the to-do list's passed task number",
      "    bump <listName> <taskIndex> [steps]   bumps the passed task to the top of the to-do list or n steps up",
      "    drop <listName> <taskIndex> [steps]   drops the passed task to the bottom of the to-do list or n steps down",
      "    help                                  Show this usage"
    ]