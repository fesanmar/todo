module Command.Dispatcher ( dispatch ) where

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
import Command.Dispatcher.Internal
    ( accurateTodoFileError, notSuchCommandError, usage )

type Command = String

dispatch :: Config -> [String] -> IO ()
dispatch config [] = usage
dispatch config args@(command : other)
  | command `elem` ["help", "ls", "config", "dl"] = runGeneralCommand config args
dispatch config (command : "-b" : fileName : args) = dispatch config (command : fileName : "-b" : args)
dispatch config (command : fileName : args) = do
  let file = accurateTodoFile fileName (defaultList config)
  (todoFile, fileExists) <- listExistsOnDir (path config) file
  if fileExists || not fileExists && command == "new"
    then runCommandOnList command $ todoFile : args
    else accurateTodoFileError fileName
dispatch todoDir (command:xs) = notSuchCommandError command

-- |Runs commands not referred to a concrete to-do list or a configuration command.
runGeneralCommand :: Config -> [String] -> IO ()
runGeneralCommand config ["help"] = usage
runGeneralCommand config ["ls"] = viewTodos (path config)
runGeneralCommand config ["config"] =
  let configLst = configToList config
   in mapM_ putStrLn configLst
        >> when (null configLst) (putStrLn "Any configuration setted yet.")
runGeneralCommand config ["dl", fileName] = do
  (_, fileExists) <- listExistsOnDir (path config) fileName
  if fileExists
    then dumpConfig $ newDefaultList fileName config
    else putErrorLn $ fileName ++ " doesn't exists. You should creat it firs using <new> command"
runGeneralCommand _ (command:xs) = notSuchCommandError command

-- |Runs commands not referred to a concrete to-do list.
runCommandOnList :: Command -> [String] -> IO ()
runCommandOnList "new" [fileName] = new fileName
runCommandOnList "remove" [fileName] = remove fileName
runCommandOnList "rename" [fileName, newFileName] = rename fileName newFileName
runCommandOnList "view" [fileName] = view fileName
runCommandOnList "add" [fileName, "-b", todoItem] = prepend fileName todoItem
runCommandOnList "add" [fileName, todoItem] = append fileName todoItem
runCommandOnList "complete" args@[fileName, numberString] = complete args
runCommandOnList "bump" [fileName, numberString] = bump fileName numberString Nothing
runCommandOnList "bump" [fileName, numberString, stepsStr] = bump fileName numberString (Just stepsStr)
runCommandOnList "drop" [fileName, numberString] = dropTask fileName numberString Nothing
runCommandOnList "drop" [fileName, numberString, stepsStr] = dropTask fileName numberString (Just stepsStr)
runCommandOnList command _ = notSuchCommandError command

accurateTodoFile :: String -> Maybe String -> String
accurateTodoFile "--" (Just file) = file
accurateTodoFile "--" Nothing = ""
accurateTodoFile file _ = file