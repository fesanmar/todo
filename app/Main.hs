module Main where

import Todo ( add, bump, new, complete, view )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist, getAppUserDataDirectory, doesDirectoryExist, createDirectoryIfMissing )
import System.FilePath ( joinPath )

type Command = String

todoDirName :: String
todoDirName = ".todo"

todoExtension :: String
todoExtension = ".todo"

main :: IO ()
main = do
    todoDir <- createTodoDirIfMissin
    arguments <- getArgs
    dispatch todoDir arguments
    
createTodoDirIfMissin :: IO FilePath 
createTodoDirIfMissin = do
    todoDir <- getAppUserDataDirectory todoDirName
    existsDir <- doesDirectoryExist todoDir
    createDirectoryIfMissing existsDir todoDir
    return todoDir

dispatch :: FilePath -> [String] -> IO ()
dispatch todoDir [] = usage
dispatch todoDir ["help"] = usage
dispatch todoDir (command:fileName:args) = 
    let todoFile = joinPath [todoDir, fileName ++ todoExtension] in do
    fileExists <- doesFileExist todoFile
    if fileExists || not fileExists && command == "new"
    then 
        runTaskLevelCommand command $ todoFile : args
    else
        errorAndUsage $ "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"
dispatch todoDir [command] = notExistingCommandError command

runTaskLevelCommand :: Command -> [String] -> IO ()
runTaskLevelCommand "new" [fileName] = new fileName
runTaskLevelCommand "view" [fileName] = view fileName
runTaskLevelCommand "add" args@[fileName, todoItem] = add args
runTaskLevelCommand "complete" args@[fileName, numberString] = complete args
runTaskLevelCommand "bump" args@[fileName, numberString] = bump args
runTaskLevelCommand command _ = notExistingCommandError command

notExistingCommandError :: String -> IO ()
notExistingCommandError command = errorAndUsage $ "There is no " ++ "<" ++ command ++ "> command or it's arguments doesn't match.\nPlease check usage:\n"

errorAndUsage :: String -> IO ()
errorAndUsage msg = putStrLn msg >> usage

usage :: IO ()
usage = mapM_ putStrLn [ "usage: todo <command> [<args>]\n"
                       , "These are the todo commands:"
                       , "    new <todoListName>                  Creates a new to-do list"
                       , "    view <todoListName>                 Show a to-do list's tasks"
                       , "    add <todoListName> <task>           add a new task to the passed to-do list"
                       , "    complete <todoListName> <taskIndex> complete the to-do list's passed task number"
                       , "    bump <todoListName> <taskIndex>     bumps the passed task to the top of the to-do list"
                       , "    help                                Show this usage"]
