module Todo.ListSpec where

import Test.Hspec ( shouldBe, it, Spec, describe )
import System.FilePath ( joinPath )
import System.Directory ( doesFileExist )
import System.IO.Silently ( capture )
import TestFixtures ( loadTestConfig, cleanUpDir )
import App.Config ( Config(path) )
import Todo.List ( new, remove, rename )
import Todo.List.Internal ( alreadyExistsListError )

spec :: Spec
spec = do
    describe "new" $ do

      it "Creating a new to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       new todoLst
       exist <- doesFileExist todoLst
       exist `shouldBe` True
       cleanUpDir
    
      it "Creating yet existing to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       new todoLst
       (output, _) <- capture $ new todoLst
       (outputError, _) <- capture $ alreadyExistsListError "work"
       cleanUpDir
       output `shouldBe` outputError
    
    describe "remove" $ do

      it "Removing a not existing to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       (output, _) <- capture $ remove todoLst
       output `shouldBe` ""
       cleanUpDir
    
      it "Removing an existing to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       new todoLst
       remove todoLst
       exist <- doesFileExist todoLst
       exist `shouldBe` False
       cleanUpDir
    
    describe "rename" $ do

      it "Renaming an existing to-do list with a not existing to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           otherName = joinPath [path config, "job.todo"]
       new todoLst
       rename todoLst "job"
       existOriginal <- doesFileExist todoLst
       existRenamed <- doesFileExist otherName
       (existOriginal, existRenamed) `shouldBe` (False, True)
       cleanUpDir
    
      it "Renaming a not existing to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       (output, _) <- capture $ rename todoLst "job"
       output `shouldBe` ""
       cleanUpDir
    
      it "Renaming an existing to-do list with another existing to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           otherName = "home"
           otherLst = joinPath [path config, otherName ++ ".todo"]
       new todoLst
       new otherLst
       (output, _) <- capture $ rename todoLst otherName
       (errorOutput, _) <- capture $ alreadyExistsListError "home"
       output `shouldBe` errorOutput
       cleanUpDir