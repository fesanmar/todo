module Todo.Transaction
    ( up
    , down
    , getTodoItems
    , getItem
    , replaceFileContent
    , putErrorLn
    , scroll
    , Direction (Up, Down)
    , TodoTask
    , NumberString )
where

import qualified Data.List as L
import Data.Char ( isDigit )
import Data.Maybe (isJust, fromJust)
import System.FilePath ( takeDirectory )
import Control.Exception ( bracketOnError )
import System.IO ( hClose, hPutStr, openTempFile )
import System.Directory ( removeFile, renameFile )

type TodoTask = String

type NumberString = String

type ListRefactor = (Int -> Int -> [TodoTask] -> [TodoTask])

data TodoConverter = TodoConverter
  { onError :: IO (),
    onSuccess :: Maybe ([TodoTask] -> TodoTask -> String)
  }

data Direction = Down | Up

putErrorLn :: Show a => a -> IO ()
putErrorLn numberString = putStrLn $ show numberString ++ " is out of bound or is not a number!"

scroll :: FilePath -> Direction -> NumberString -> Maybe NumberString -> IO ()
scroll fileName dir indexStr maybeSteps =
  let (stepsConvF, noStepsConvF) = case dir of
        Up -> (up, \items itemToBump -> unlines $ itemToBump : L.delete itemToBump items)
        Down -> (down, \items itemToDrop -> unlines $ L.delete itemToDrop items ++ [itemToDrop])
      accurateFuncs = case maybeSteps of
        Nothing -> TodoConverter (putErrorLn indexStr) $ Just noStepsConvF
        Just steps -> accurateTodoConv stepsConvF indexStr steps
   in bracketOnItemExists fileName indexStr (onError accurateFuncs) (onSuccess accurateFuncs)

up :: Int -> Int -> [TodoTask] -> [TodoTask]
up index steps items
  | steps <= 0 || index <= 0 || index >= length items = items
  | otherwise = up prev (steps -1) oneStepUpList
  where
    oneStepUpList = take prev items ++ (items !! index) : items !! prev : drop next items
    prev = index - 1
    next = index + 1

down :: Int -> Int -> [TodoTask] -> [TodoTask]
down index steps items = reverse . up reverseIndex steps $ reverse items
  where
    reverseIndex = length items - index - 1

accurateTodoConv :: ListRefactor -> NumberString -> NumberString -> TodoConverter
accurateTodoConv f indexStr stepsStr =
  if all isDigit stepsStr
    then TodoConverter (putErrorLn indexStr) $ Just (\items _ -> unlines $ f (read indexStr) (read stepsStr) items)
    else TodoConverter (putErrorLn "steps") Nothing

bracketOnItemExists :: FilePath -> NumberString -> IO () -> Maybe ([TodoTask] -> TodoTask -> String) -> IO ()
bracketOnItemExists fileName numberString ioError fSuccess = do
  items <- getTodoItems fileName
  let itemMaybe = getItem numberString items
  if isJust itemMaybe && isJust fSuccess
    then
      let itemToOp = fromJust itemMaybe
       in replaceFileContent fileName $ fromJust fSuccess items itemToOp
    else ioError

getTodoItems :: FilePath -> IO [TodoTask]
getTodoItems fileName = do
  contents <- readFile fileName
  return $ lines contents

getItem :: NumberString -> [TodoTask] -> Maybe TodoTask
getItem "" _ = Nothing
getItem stringNumber xs = getItem' =<< getNumberMaybe
  where
    getNumberMaybe :: Maybe Int
    getNumberMaybe = if isNumber then Just (read stringNumber) else Nothing
    isNumber = all isDigit stringNumber
    getItem' n = if n `elem` zipWith const [0 ..] xs then Just (xs !! n) else Nothing

replaceFileContent :: FilePath -> String -> IO ()
replaceFileContent fileName newContent =
  let todoDir = takeDirectory fileName
   in bracketOnError
        (openTempFile todoDir "temp")
        ( \(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName
        )
        ( \(tempName, tempHandle) -> do
            hPutStr tempHandle newContent
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName
        )