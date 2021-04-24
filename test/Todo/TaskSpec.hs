module Todo.TaskSpec where

import Test.Hspec ( shouldBe, it, Spec, describe )
import System.FilePath ( joinPath )
import Data.Either ( isLeft, isRight )
import TestFixtures ( loadTestConfig, cleanUpDir )
import App.Config ( Config(path) )
import Todo.List ( new )
import Todo.Task ( append, prepend )

spec :: Spec
spec = do
    describe "append" $ do
      it "Appending a new task into a not existing list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       output <- append todoLst "Some task"
       isLeft output `shouldBe` True
       cleanUpDir
    
      it "Appending a new task into an empty list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           someTask = "Some task"
       new todoLst
       output <- append todoLst someTask
       task <- readFile todoLst
       (isRight output, task) `shouldBe` (True, someTask ++ "\n")
       cleanUpDir
    
      it "Appending a new task into a not empty list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           firstTask = "First task"
           secondTask = "Second task"
       new todoLst
       append todoLst firstTask
       output <- append todoLst secondTask
       tasks <- readFile todoLst
       let [_, sec] = lines tasks
       (isRight output, sec) `shouldBe` (True, secondTask)
       cleanUpDir

      it "Appending an empty task into a list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           emptyTask = ""
       new todoLst
       output <- append todoLst emptyTask
       isLeft output `shouldBe` True
       cleanUpDir

      it "Appending a space only task into a list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           spacedTask = "  "
       new todoLst
       output <- append todoLst spacedTask
       isLeft output `shouldBe` True
       cleanUpDir

    describe "prepend" $ do
      
      it "Prepending a new task into a not existing list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       output <- prepend todoLst "Some task"
       isLeft output `shouldBe` True
       cleanUpDir
    
      it "Prepending a new task into an empty list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           someTask = "Some task"
       new todoLst
       output <- prepend todoLst someTask
       task <- readFile todoLst
       (isRight output, task) `shouldBe` (True, someTask ++ "\n")
       cleanUpDir
    
      it "Prepending a new task into a not empty list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           firstTask = "First task"
           secondTask = "Second task"
       new todoLst
       append todoLst firstTask
       output <- prepend todoLst secondTask
       tasks <- readFile todoLst
       let [first, _] = lines tasks
       (isRight output, first) `shouldBe` (True, secondTask)
       cleanUpDir

      it "Prepending an empty task into a list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           emptyTask = ""
       new todoLst
       output <- prepend todoLst emptyTask
       isLeft output `shouldBe` True
       cleanUpDir

      it "Prepending a space only task into a list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           spacedTask = "  "
       new todoLst
       output <- prepend todoLst spacedTask
       isLeft output `shouldBe` True
       cleanUpDir