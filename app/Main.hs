module Main where

import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getAppUserDataDirectory,
  )
import System.Environment ( getArgs )
import Util.Console ( putErrorLn )
import Config ( loadConfig, Config(path) )
import Todo.List ( new, remove, rename, todoFilePath, viewTodos )
import Todo.Task ( append, bump, complete, dropTask, prepend, view )

type Command = String

todoDirName :: String
todoDirName = ".todo"

main :: IO ()
main = do
  todoDir <- getAppUserDataDirectory todoDirName
  config <- loadConfig todoDir
  arguments <- getArgs
  dispatch config arguments

dispatch :: Config  -> [String] -> IO ()
dispatch config [] = usage
dispatch config ["help"] = usage
dispatch config ["ls"] = viewTodos (path config)
dispatch config (command : "-b" : fileName : args) = dispatch config (command : fileName : "-b" : args)
dispatch config (command : fileName : args) = do
  (todoFile, fileExists) <- listExistsOnDir (path config) fileName
  if fileExists || not fileExists && command == "new"
    then runCommand command $ todoFile : args
    else putStrLn $ "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"
dispatch todoDir [command] = notExistingCommandError command

listExistsOnDir :: FilePath -> FilePath -> IO (FilePath, Bool)
listExistsOnDir todoDir fileName =
  let todoFile = todoFilePath todoDir fileName
   in doesFileExist todoFile
      >>= \fileExists -> return (todoFile, fileExists)

runCommand :: Command -> [String] -> IO ()
runCommand "new" [fileName] = new fileName
runCommand "remove" [fileName] = remove fileName
runCommand "rename" [fileName, newFileName] = rename fileName newFileName
runCommand "view" [fileName] = view fileName
runCommand "add" [fileName, "-b", todoItem] = prepend fileName todoItem
runCommand "add" [fileName, todoItem] = append fileName todoItem
runCommand "complete" args@[fileName, numberString] = complete args
runCommand "bump" args@[fileName, numberString] = bump fileName numberString Nothing
runCommand "bump" args@[fileName, numberString, stepsStr] = bump fileName numberString (Just stepsStr)
runCommand "drop" args@[fileName, numberString] = dropTask fileName numberString Nothing
runCommand "drop" args@[fileName, numberString, stepsStr] = dropTask fileName numberString (Just stepsStr)
runCommand command _ = notExistingCommandError command

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
      "    ls                                        Shows user's to-do lists",
      "    new <todoListName>                        Creates a new to-do list",
      "    remove <todoListName>                     removes a existing to-do list",
      "    rename <todoListName> <newName>           renames a existing to-do list",
      "    view <todoListName>                       Shows a to-do list's tasks",
      "    add [-b] <todoListName> <task>            append a new task to the passed to-do list or prepend it if [-b] is setted",
      "    complete <todoListName> <taskIndex>       complete the to-do list's passed task number",
      "    bump <todoListName> <taskIndex> [steps]   bumps the passed task to the top of the to-do list or n steps up",
      "    drop <todoListName> <taskIndex> [steps]   drops the passed task to the bottom of the to-do list or n steps down",
      "    help                                      Show this usage"
    ]
