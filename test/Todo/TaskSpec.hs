{-# LANGUAGE OverloadedStrings #-}
module Todo.TaskSpec where

import Test.Hspec ( shouldBe, it, Spec, describe )
import System.FilePath ( joinPath )
import Data.Either ( isLeft, isRight )
import qualified Data.ByteString as S
import qualified Data.ByteString.UTF8 as BUT
import System.IO.Silently ( capture )
import TestFixtures ( loadTestConfig, cleanUpDir, cleanOutput )
import App.Config ( Config(path) )
import Todo.List ( new )
import Todo.FileHandling.Internal ( noSuchTodoList )
import Todo.Task ( append, prepend, view, complete, mv )
import Todo.Task.Internal
    ( taskMovedMsg,
      taskCompletedMsg,
      outOfBoundErrorMsg,
      emptyListMsg )

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
    
    describe "view" $ do
      
      it "View a not existing list" $ do
       config <- loadTestConfig
       let inexistingTodoLst = joinPath [path config, "work.todo"]
       output <- view inexistingTodoLst
       isLeft output `shouldBe` True 
       cleanUpDir
      
      it "View an existing empty list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       new todoLst
       output <- view todoLst
       output `shouldBe` Right (emptyListMsg todoLst)
       cleanUpDir
      
      it "View an existing list with one task inside" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           someTask = "Some task"
       new todoLst
       append todoLst someTask
       output <- view todoLst
       output `shouldBe` Right ("0 - " ++ someTask ++ "\n")
       cleanUpDir
      
      it "View an existing list with more than one task inside" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
           someTask = "Some task"
           otherTask = "Some other task"
       new todoLst
       append todoLst someTask
       append todoLst otherTask
       output <- view todoLst
       output `shouldBe` Right ("0 - " ++ someTask ++ "\n" ++ "1 - " ++ otherTask ++ "\n")
       cleanUpDir

    describe "complete" $ do
      
      it "Completing from a not existing list" $ do
       config <- loadTestConfig
       let lstName = "work"
           todoLst = joinPath [path config, lstName ++ ".todo"]
           taskPosition = "0"
       output <- complete todoLst taskPosition
       output `shouldBe` Left (noSuchTodoList lstName)
       cleanUpDir
      
      it "Completing from an existing empty list" $ do
       config <- loadTestConfig
       let lstName = "work"
           todoLst = joinPath [path config, lstName ++ ".todo"]
           taskPosition = "0"
       new todoLst
       output <- complete todoLst taskPosition
       output `shouldBe` Left (outOfBoundErrorMsg taskPosition)
       cleanUpDir
      
      it "Completing an out of bound task from an singleton list" $ do
       config <- loadTestConfig
       let lstName = "work"
           todoLst = joinPath [path config, lstName ++ ".todo"]
           someTask = "Some task"
           taskPosition = "1"
       new todoLst
       append todoLst someTask
       output <- complete todoLst taskPosition
       output `shouldBe` Left (outOfBoundErrorMsg taskPosition)
       cleanUpDir
      
      it "Completing a task from an singleton list" $ do
       config <- loadTestConfig
       let lstName = "work"
           todoLst = joinPath [path config, lstName ++ ".todo"]
           someTask = "Some task"
           taskPosition = "0"
       new todoLst
       append todoLst someTask
       output <- complete todoLst taskPosition
       output `shouldBe` Right (taskCompletedMsg someTask)
       cleanUpDir
      
      it "Completing a task from an binary list" $ do
       config <- loadTestConfig
       let lstName = "work"
           todoLst = joinPath [path config, lstName ++ ".todo"]
           someTask = "Some task"
           otherTask = "Other task"
           taskPosition = "1"
       new todoLst
       append todoLst someTask
       append todoLst otherTask
       output <- complete todoLst taskPosition
       output `shouldBe` Right (taskCompletedMsg otherTask)
       cleanUpDir
    
    describe "mv" $ do
      
      it "Moving a task from a not existing list to a not existinig list" $ do
       config <- loadTestConfig
       let lstFromName = "work"
           lstToName = "job"
           todoLstFrom = joinPath [path config, lstFromName ++ ".todo"]
           todoLstTo = joinPath [path config, lstToName ++ ".todo"]
           taskPosition = "0"
       output <- mv todoLstFrom taskPosition todoLstTo
       output `shouldBe` Left (noSuchTodoList lstFromName)
       cleanUpDir
      
      it "Moving a task from an existing list to a not existinig list" $ do
       config <- loadTestConfig
       let lstFromName = "work"
           lstToName = "job"
           todoLstFrom = joinPath [path config, lstFromName ++ ".todo"]
           todoLstTo = joinPath [path config, lstToName ++ ".todo"]
           taskPosition = "0"
       new todoLstFrom
       output <- mv todoLstFrom taskPosition todoLstTo
       output `shouldBe` Left (noSuchTodoList lstToName)
       cleanUpDir
      
      it "Moving an out of bound task from a list to another" $ do
       config <- loadTestConfig
       let lstFromName = "work"
           lstToName = "job"
           todoLstFrom = joinPath [path config, lstFromName ++ ".todo"]
           todoLstTo = joinPath [path config, lstToName ++ ".todo"]
           taskPosition = "0"
       new todoLstFrom
       new todoLstTo
       output <- mv todoLstFrom taskPosition todoLstTo
       output `shouldBe` Left (outOfBoundErrorMsg taskPosition)
       cleanUpDir
      
      it "Moving a task from a list to another" $ do
       config <- loadTestConfig
       let lstFromName = "work2"
           lstToName = "job2"
           todoLstFrom = joinPath [path config, lstFromName ++ ".todo"]
           todoLstTo = joinPath [path config, lstToName ++ ".todo"]
           task = "Some task"
           taskPosition = "0"
       new todoLstFrom
       append todoLstFrom task
       new todoLstTo
       output <- mv todoLstFrom taskPosition todoLstTo
       lstToTask <- S.readFile todoLstTo
       lstFromTask <- S.readFile todoLstFrom
       let cleanedOutput = cleanOutput <$> output
           cleanedLstToTask = cleanOutput $ BUT.toString lstToTask
       (cleanedOutput, cleanedLstToTask, lstFromTask) `shouldBe` 
                (Right $ taskMovedMsg task todoLstTo, task ++ "\n", "")
       cleanUpDir
      
      it "Moving a task with a wrong index" $ do
       config <- loadTestConfig
       let lstFromName = "work3"
           lstToName = "job3"
           todoLstFrom = joinPath [path config, lstFromName ++ ".todo"]
           todoLstTo = joinPath [path config, lstToName ++ ".todo"]
           task = "Some task"
           taskPosition = "not an index"
       new todoLstFrom
       append todoLstFrom task
       new todoLstTo
       output <- mv todoLstFrom taskPosition todoLstTo
       output `shouldBe` Left (outOfBoundErrorMsg taskPosition)
       cleanUpDir