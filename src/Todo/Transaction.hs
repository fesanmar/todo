module Todo.Transaction
    ( lastIndex
    , getTodoItems
    , getItem
    , replaceFileContent
    , outOfBoundError
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
import Data.List.Scroll ( down, up )
import Util.Console ( putErrorLn )

type TodoTask = String

type NumberString = String

type ListRefactor = (Int -> Int -> [TodoTask] -> [TodoTask])

data TodoConverter = TodoConverter
  { onError :: IO (),
    onSuccess :: Maybe ([TodoTask] -> TodoTask -> String)
  }

data Direction = Down | Up

outOfBoundError :: NumberString -> IO ()
outOfBoundError numberString = 
  putErrorLn $ show numberString ++ " is out of bound or is not a number!"

lastIndex :: FilePath -> IO Int
lastIndex fileName = getTodoItems fileName >>= \list -> return $ length list - 1

scroll :: FilePath -> Direction -> NumberString -> Maybe NumberString -> IO ()
scroll fileName dir indexStr maybeSteps =
  let (stepsConvF, noStepsConvF) = case dir of
        Up -> (up, \items itemToBump -> unlines $ itemToBump : L.delete itemToBump items)
        Down -> (down, \items itemToDrop -> unlines $ L.delete itemToDrop items ++ [itemToDrop])
      accurateFuncs = case maybeSteps of
        Nothing -> TodoConverter (outOfBoundError indexStr) $ Just noStepsConvF
        Just steps -> accurateTodoConv stepsConvF indexStr steps
   in bracketOnItemExists fileName indexStr (onError accurateFuncs) (onSuccess accurateFuncs)

accurateTodoConv :: ListRefactor -> NumberString -> NumberString -> TodoConverter
accurateTodoConv f indexStr stepsStr =
  if all isDigit stepsStr
    then TodoConverter (outOfBoundError indexStr) $ Just (\items _ -> unlines $ f (read indexStr) (read stepsStr) items)
    else TodoConverter (outOfBoundError "steps") Nothing

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