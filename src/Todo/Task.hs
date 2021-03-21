{-# LANGUAGE OverloadedStrings #-}
module Todo.Task
    ( view
    , append
    , prepend
    , complete
    , up
    , down
    , bump
    , dropTask
    ) where

import           Control.Exception (bracketOnError)
import           Data.Char         (isDigit)
import qualified Data.List         as L
import           Data.Maybe        (fromJust, isJust, isNothing)
import qualified Data.Text         as T
import           System.Directory  (removeFile, renameFile)
import           System.FilePath   (takeDirectory, takeFileName)
import           System.IO         (hClose, hPutStr, openTempFile, stderr)
import           Todo.List         (nameFromPath)

type TodoTask = String
type NumberString = String

append :: FilePath -> String -> IO ()
append fileName todoItem = appendFile fileName $ todoItem ++ "\n"

prepend :: FilePath -> String -> IO ()
prepend fileName todoItem =
    append fileName todoItem
    >> lastIndex fileName
    >>= \index -> bump [fileName, show index]

lastIndex :: FilePath -> IO Int
lastIndex fileName = getTodoItems fileName >>= \list -> return $ length list - 1

view :: String -> IO ()
view fileName = do
    todoTasks <- getTodoItems fileName
    let isEmpty = null todoTasks || all (T.null . T.strip . T.pack ) todoTasks
    if isEmpty then putStrLn $ nameFromPath fileName ++ " is empty."
    else printListedTask todoTasks

printListedTask :: [TodoTask] -> IO ()
printListedTask todoTasks = let numberedTasks = zipWith (\n task -> show n ++ " - " ++ task) [0..] todoTasks in
                            mapM_ putStrLn numberedTasks

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

down :: Int -> Int -> [TodoTask] -> [TodoTask]
down index steps items = reverse . up reverseIndex steps $ reverse items
    where reverseIndex = length items - index - 1

up :: Int -> Int -> [TodoTask] -> [TodoTask]
up index steps items
    | steps <= 0 || index <= 0 || index >= length items= items
    | otherwise = up prev (steps -1) oneStepUpList
    where oneStepUpList = take prev items ++ (items !! index) : items !! prev : drop next items
          prev = index - 1
          next = index + 1

bump :: [String] -> IO ()
bump [fileName, numberString] =
  bracketOnItemExists
    fileName
    numberString
    (putErrorLn numberString)
    (\items itemToBump -> unlines $ itemToBump : L.delete itemToBump items)

dropTask :: [String] -> IO ()
dropTask [fileName, numberString] =
  bracketOnItemExists
    fileName
    numberString
    (putErrorLn numberString)
    (\items itemToDrop -> unlines $ L.delete itemToDrop items ++ [itemToDrop])

bracketOnItemExists :: FilePath -> NumberString -> IO () -> ([TodoTask] -> TodoTask -> String) -> IO ()
bracketOnItemExists fileName numberString ioError fSuccess = do
  items <- getTodoItems fileName
  let itemMaybe = getItem numberString items
  if isJust itemMaybe
    then let itemToOp = fromJust itemMaybe
          in replaceFileContent fileName $ fSuccess items itemToOp
    else ioError

getTodoItems :: FilePath -> IO [TodoTask]
getTodoItems fileName = do
    contents <- readFile fileName
    return $ lines contents

getItem :: NumberString -> [TodoTask] -> Maybe TodoTask
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
