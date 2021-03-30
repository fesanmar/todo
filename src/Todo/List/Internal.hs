module Todo.List.Internal where

import Util.Console (putErrorLn)

alreadyExistsListError :: FilePath -> IO ()
alreadyExistsListError fileName = putErrorLn $ "Already exist a to-do list with the name " ++ "[" ++ fileName ++ "]"