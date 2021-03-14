module Main where

import Todo ( add, bump, new, remove, view )
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
    (command:fileName:args) <- getArgs
    todoDir <- getAppUserDataDirectory todoDirName
    existsDir <- doesDirectoryExist todoDir
    createDirectoryIfMissing existsDir todoDir
    let todoFile = joinPath [todoDir, fileName ++ todoExtension]
    fileExist <- doesFileExist todoFile
    if fileExist || not fileExist && command == "new"
    then do
        dispatch command $ todoFile : args
    else do
        putStrLn $ "There is no todo list with the name " ++ "[" ++ fileName ++ "]. You should create it firs using <new> commmand"

dispatch :: Command -> [String] -> IO ()
dispatch "new" [fileName] = new fileName
dispatch "view" [fileName] = view fileName
dispatch "add" args@[fileName, todoItem] = add args
dispatch "remove" args@[fileName, numberString] = remove args
dispatch "bump" args@[fileName, numberString] = bump args
dispatch command _ = usage

usage :: IO ()
usage = putStrLn "Usage here"
