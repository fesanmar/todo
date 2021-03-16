{-# LANGUAGE OverloadedStrings #-}
module Todo
    ( todoExtension
    , view
    , append
    , prepend
    , complete
    , bump
    , new
    ) where

import System.Directory (removeFile, renameFile, doesFileExist )
import System.IO ( hClose, hPutStr, openTempFile, openFile, IOMode (WriteMode) )
import System.FilePath ( takeDirectory, takeFileName )
import Control.Exception ( bracketOnError )
import qualified Data.List as L
import Data.Maybe ( fromJust, isJust )
import Data.Char ( isDigit )
import qualified Data.Text as T

todoExtension :: String
todoExtension = ".todo"

extensionLen :: Int
extensionLen = length todoExtension

new :: FilePath -> IO ()
new fileName = do
    fileExist <- doesFileExist fileName
    if fileExist
    then putStrLn $ "Already exist a to-do list with the name " ++ "[" ++ fileName ++ "]"
    else do
        handle <- openFile fileName WriteMode
        hClose handle

append :: FilePath -> String -> IO ()
append fileName todoItem = appendFile fileName $ todoItem ++ "\n" 

prepend :: FilePath -> String -> IO ()
prepend fileName todoItem = 
    append fileName todoItem 
    >> lastIndex fileName
    >>= \index -> bump [fileName, show index]

lastIndex :: FilePath -> IO Int
lastIndex fileName = 
    getTodoItems fileName 
    >>= \list -> return $ length list - 1

view :: String -> IO ()
view fileName = do
    todoTasks <- getTodoItems fileName
    let isEmpty = null todoTasks || all (T.null . T.strip . T.pack ) todoTasks
    if isEmpty then putStrLn $ nameFromPath fileName ++ " is empty."
    else printListedTask todoTasks

printListedTask :: [String] -> IO ()
printListedTask todoTasks = let numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [0..] todoTasks in
                            mapM_ putStrLn numberedTasks

nameFromPath :: FilePath -> String 
nameFromPath fileName = T.unpack . T.dropEnd extensionLen $ T.pack $ takeFileName fileName

complete :: [String] -> IO ()
complete [fileName, numberString] = do
    items <- getTodoItems fileName
    let itemToDelete = getItem numberString items
    if isJust itemToDelete 
    then
        replaceFileContent fileName (unlines $ L.delete (fromJust itemToDelete) items)
    else
        putErrorLn numberString

putErrorLn :: Show a => a -> IO ()
putErrorLn numberString = putStrLn $ show numberString ++ " is out of bound or is not a number!"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    items <- getTodoItems fileName
    let itemToBumpMaybe = getItem numberString items
    if isJust itemToBumpMaybe
    then
        let itemToBump = fromJust itemToBumpMaybe in
        replaceFileContent fileName (unlines $ itemToBump : L.delete itemToBump items)
    else
         putErrorLn numberString

getTodoItems :: FilePath -> IO [String]
getTodoItems fileName = do
    contents <- readFile fileName
    return $ lines contents

getItem :: String -> [String] -> Maybe String
getItem "" _ = Nothing
getItem stringNumber xs = getItem' =<< getNumberMaybe
    where getNumberMaybe :: Maybe Int
          getNumberMaybe = if isNumber then Just (read stringNumber) else Nothing 
          isNumber = all isDigit stringNumber
          getItem' n = if n `elem` zipWith const [0..] xs then Just (xs !! n) else Nothing

replaceFileContent :: FilePath -> String -> IO ()
replaceFileContent fileName newContent = let todoDir =  takeDirectory fileName in
                                         bracketOnError (openTempFile todoDir "temp") 
                                            (\(tempName, tempHandle) -> do 
                                                hClose tempHandle 
                                                removeFile tempName) 
                                            (\(tempName, tempHandle) -> do 
                                                hPutStr tempHandle newContent 
                                                hClose tempHandle 
                                                removeFile fileName 
                                                renameFile tempName fileName)