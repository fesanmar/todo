module Main where

import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getAppUserDataDirectory,
  )
import System.Environment ( getArgs )
import Util.Console ( putErrorLn )
import Config
    ( dumpConfig,
      loadConfig,
      newDefaultList,
      Config(defaultList, path) )
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
    then runCommand command $ todoFile : args
    else accurateTodoFileError fileName
dispatch todoDir [command] = notExistingCommandError command

accurateTodoFile :: String -> Maybe String -> String
accurateTodoFile "--" (Just file) = file
accurateTodoFile "--" Nothing = ""
accurateTodoFile file _ = file

accurateTodoFileError :: String -> IO ()
accurateTodoFileError "--" = errorAndUsage "There is no to-do list setted as default. Use <dl> to accomplish it." 
accurateTodoFileError fileName = putErrorLn $ "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"

listExistsOnDir :: FilePath -> FilePath -> IO (FilePath, Bool)
listExistsOnDir todoDir "" = return ("", False)
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
