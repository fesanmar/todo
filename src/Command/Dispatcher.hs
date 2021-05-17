module Command.Dispatcher ( dispatch ) where

import App.Config
    ( newDefaultList,
      dumpConfig,
      configToList,
      Config(defaultList, path), removeDefaultList, isDefaultList )
import Control.Monad ( when, guard, void )
import Todo.List
    ( new, remove, rename, viewTodos )
import Todo.Task
    ( append, prepend, view, complete, bump, dropTask )
import Todo.FileHandling ( nameFromPath, listExistsOnDir )
import Util.Console (putErrorLn)
import Command.Dispatcher.Internal
    ( accurateTodoFileError, notSuchCommandError, usage, noSuchListError )
import Data.Either (fromRight)

type Command = String

-- |Dispatch a command with the passed configuration and arguments.
dispatch :: Config -> [String] -> IO ()
dispatch config [] = usage
dispatch config args@(command:other)
  | command `elem` ["help", "ls", "config", "dl"] = runGeneralCommand config args
dispatch config (command : "-b" : fileName : args) = dispatch config (command : fileName : "-b" : args)
dispatch config (command : fileName : args) = do
  let file = accurateTodoFile fileName (defaultList config)
  (todoFile, _) <- listExistsOnDir (path config) file
  runCommandOnList config command $ todoFile : args
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
    else noSuchListError fileName
runGeneralCommand _ (command:xs) = notSuchCommandError command

-- |Runs commands not referred to a concrete to-do list.
runCommandOnList :: Config -> Command -> [String] -> IO ()
runCommandOnList config "new" [fileName] = new fileName
runCommandOnList config "remove" [fileName] =
  remove fileName
    >>= whenRight (reconfigWhen config (isDefaultList' fileName) removeDefaultList)
runCommandOnList config "rename" [fileName, newFileName] =
  rename fileName newFileName
    >>= whenRight (reconfigWhen config (isDefaultList' fileName) (newDefaultList newFileName))
runCommandOnList config "view" [fileName] = view fileName >>= putAccurateOut
runCommandOnList config "add" [fileName, "-b", todoItem] = prepend fileName todoItem >>= putErrorWhenWrong 
runCommandOnList config "add" [fileName, todoItem] = append fileName todoItem >>= putErrorWhenWrong 
runCommandOnList config "complete" args@[fileName, numberString] = complete fileName numberString >>= putAccurateOut
runCommandOnList config "bump" [fileName, numberString] = bump fileName numberString Nothing >>= putAccurateOut
runCommandOnList config "bump" [fileName, numberString, stepsStr] = bump fileName numberString (Just stepsStr) >>= putAccurateOut
runCommandOnList config "drop" [fileName, numberString] = dropTask fileName numberString Nothing >>= putAccurateOut
runCommandOnList config "drop" [fileName, numberString, stepsStr] = dropTask fileName numberString (Just stepsStr) >>= putAccurateOut
runCommandOnList config command _ = notSuchCommandError command

-- |Runs and returns the IO Command if Either is right.
whenRight :: IO () -> Either String () -> IO ()
whenRight io (Right _) = io
whenRight _ (Left _) = return ()

putErrorWhenWrong :: Either String () -> IO ()
putErrorWhenWrong (Right _) = return ()
putErrorWhenWrong (Left msg) = putErrorLn msg

-- |Prints an error line if 'Either' is 'Left' and a normal line if is 'Right'.
putAccurateOut :: Either String String -> IO ()
putAccurateOut (Right "") = return ()
putAccurateOut (Left "") = return ()
putAccurateOut (Right msg) = putStrLn msg
putAccurateOut (Left msg) = putErrorLn msg

{-|
  Given a configuration, a predicate and a functions that operates
  with the configuration, executes that function and make that change
  persistent if the predicate evaluates to 'True'.
-}
reconfigWhen :: Config -> (Config -> Bool)  -> (Config -> Config )-> IO ()
reconfigWhen cfg p f = when (p cfg) $ dumpConfig (f cfg)

-- |Returns 'True' if the first argument is the default to-do list.
isDefaultList' :: FilePath -> Config -> Bool
isDefaultList' file = isDefaultList (nameFromPath file)

accurateTodoFile :: String -> Maybe String -> String
accurateTodoFile "--" (Just file) = file
accurateTodoFile "--" Nothing = ""
accurateTodoFile file _ = file