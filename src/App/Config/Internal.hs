{-# LANGUAGE OverloadedStrings #-}
module App.Config.Internal where

import System.Directory
    ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist )
import Data.Tuple.Extra ( both, second )
import qualified Data.Text as T
import qualified Data.ByteString as Str
import qualified Data.ByteString.UTF8 as BUT
import Control.Applicative ( Applicative(liftA2) )

{-|
  Checks that the file passed as an argument exists, otherwise 
  creates it.
-}
createTodoDirIfMissing :: FilePath -> IO FilePath
createTodoDirIfMissing todoDir =
  doesDirectoryExist todoDir
  >>= \existsDir -> createDirectoryIfMissing existsDir todoDir
  >> return todoDir

-- |Extracts the content form an ini file.
iniContent :: FilePath -> IO Str.ByteString
iniContent iniFile =
  doesFileExist iniFile
    >>= \exists ->
      if exists
        then Str.readFile iniFile
        else return ""

{-|
  Extracts the key a values from a ini file content and returns
  inside a list of tuples.

  An example is given below:

  >>> let content = "; comment\n[Section]\nmyKey=value\n;commentLike=keyValue\nnothing, ignore me.\n"
  >>> extractKVPairs $ Data.ByteString.UTF8.fromString content
  [("myKey","value")]
-}
extractKVPairs :: Str.ByteString -> [(String, String)]
extractKVPairs = toAssocList . removeNoKeyValueLines . textLines
  where textLines = map T.pack . lines . BUT.toString
        removeNoKeyValueLines = filter isKeyValuePair
        isKeyValuePair = liftA2 (&&) ("=" `T.isInfixOf`) $ not . isCommentOrSection
        isCommentOrSection = liftA2 (||) (";" `T.isPrefixOf`) ("[" `T.isPrefixOf`)
        toAssocList = map (both (T.unpack . T.strip) . second (T.drop 1) . T.break (== '='))
