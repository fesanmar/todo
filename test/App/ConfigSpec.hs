module App.ConfigSpec where

import Test.Hspec ( shouldBe, it, Spec, describe )
import TestFixtures
    ( loadTestConfig,
      cleanUpDir,
      iniFilePath,
      basePath,
      configAndCleanUpDir )
import qualified Data.ByteString.UTF8 as BUT
import App.Config
    ( configToList,
      dumpConfig,
      loadConfig,
      newDefaultList,
      Config(path, configFilePath, defaultList) )
import App.Config.Internal ( extractKVPairs )

spec :: Spec
spec = do
    describe "extractKVPairs" $ do
      it "Fetching key value pairs form ini file content" $
       let content = "; comment\n[Section]\nmyKey=value\n;commentLike=keyValue\nnothing, ignore me.\n"
           keyValuePairs = extractKVPairs $ BUT.fromString content in
       keyValuePairs `shouldBe` [("myKey","value")]

    describe "loadConfig" $ do
      it "Loading config creates .todo dir inside base path" $ do
       config <- configAndCleanUpDir
       path config `shouldBe` basePath 
    
      it "Loading config set ini.todo file" $ do
       config <- loadConfig basePath 
       configFilePath config `shouldBe` iniFilePath
       cleanUpDir
    
      it "Loading config default todo list as Nothing if todo.ini doesn't exists" $ do
       config <- configAndCleanUpDir
       defaultList config `shouldBe` Nothing 
    
    describe "default to-do list" $ do
      it "Seting default to-do list, saving config and reloading it" $ do
       config <- loadTestConfig
       let modifiedConfig = newDefaultList "work" config
       dumpConfig modifiedConfig
       newLoadedConfig <- loadTestConfig
       newLoadedConfig `shouldBe` modifiedConfig
       cleanUpDir

    describe "configToList" $ do
      it "Casting empty config to list" $ do
       config <- configAndCleanUpDir
       configToList config `shouldBe` []

      it "Casting a not empty config to list" $ do
       config <- configAndCleanUpDir
       let modifiedConfig = newDefaultList "work" config
       configToList modifiedConfig `shouldBe` ["defaultList=work"]