module Command.Dispatcher.Internal where

import Util.Console ( putErrorLn )
import App.Messages ( noSuchTodoList, emptyTaskMsg )

accurateTodoFileError :: String -> IO ()
accurateTodoFileError "--" = errorAndUsage "There is no to-do list setted as default. Use <dl> to accomplish it." 
accurateTodoFileError fileName = noSuchListError fileName

notSuchCommandError :: String -> IO ()
notSuchCommandError command = errorAndUsage $ "There is no " ++ "<" ++ command ++ "> command or it's arguments doesn't match.\nPlease check usage:\n"

noSuchListError :: String -> IO ()
noSuchListError fileName = putErrorLn $ noSuchTodoList fileName

emptyTaskError :: IO ()
emptyTaskError = putErrorLn emptyTaskMsg  

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