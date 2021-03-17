module Main where

import Todo.Task ( append, prepend, bump, complete, view )
import Todo.List ( todoExtension, new, viewTodos )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist, getAppUserDataDirectory, doesDirectoryExist, createDirectoryIfMissing )
import System.FilePath ( joinPath )

type Command = String

todoDirName :: String
todoDirName = ".todo"

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
dispatch todoDir ["ls"] = viewTodos todoDir
dispatch todoDir (command:"-b":fileName:args) = dispatch todoDir (command : fileName : "-b" : args)
dispatch todoDir (command:fileName:args) = do
    (todoFile, fileExists) <- listExistsOnDir todoDir fileName
    if fileExists || not fileExists && command == "new"
    then 
        runCommand command $ todoFile : args
    else
        errorAndUsage $ "There is no to-do list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"
dispatch todoDir [command] = notExistingCommandError command

listExistsOnDir :: FilePath -> FilePath -> IO (FilePath, Bool)
listExistsOnDir todoDir fileName =
    let todoFile = joinPath [todoDir, fileName ++ todoExtension] in do
    fileExists <- doesFileExist todoFile
    return (todoFile, fileExists)

runCommand :: Command -> [String] -> IO ()
runCommand "new" [fileName] = new fileName
runCommand "view" [fileName] = view fileName
runCommand "add" [fileName, "-b", todoItem] = prepend fileName todoItem
runCommand "add" [fileName, todoItem] = append fileName todoItem
runCommand "complete" args@[fileName, numberString] = complete args
runCommand "bump" args@[fileName, numberString] = bump args
runCommand command _ = notExistingCommandError command

notExistingCommandError :: String -> IO ()
notExistingCommandError command = errorAndUsage $ "There is no " ++ "<" ++ command ++ "> command or it's arguments doesn't match.\nPlease check usage:\n"

errorAndUsage :: String -> IO ()
errorAndUsage msg = putStrLn msg >> usage

usage :: IO ()
usage = mapM_ putStrLn [ "usage: todo <command> [<args>]\n"
                       , "These are the todo commands:"
                       , "    new <todoListName>                  Creates a new to-do list"
                       , "    view <todoListName>                 Show a to-do list's tasks"
                       , "    add [-b] <todoListName> <task>      append a new task to the passed to-do list or prepend it if [-b] is setted."
                       , "    complete <todoListName> <taskIndex> complete the to-do list's passed task number"
                       , "    bump <todoListName> <taskIndex>     bumps the passed task to the top of the to-do list"
                       , "    help                                Show this usage"]
