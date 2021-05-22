module Command.DispatcherSpec where

import Test.Hspec ( shouldBe, it, Spec, describe )
import System.FilePath ( joinPath )
import System.Directory ( doesFileExist )
import System.IO.Silently ( capture )
import TestFixtures
    ( cleanUpDir,
      configAndCleanUpDir,
      loadTestConfig,
      outOfBoundError,
      taskMoved )
import Util.Console ( putErrorLn )
import App.Config ( Config(defaultList, path) )
import Todo.List ( new, notTodoListToShowMsg )
import Todo.List.Internal ( alreadyExistsListError )
import Todo.Task.Internal ( emptyTaskMsg, emptyListMsg )
import Todo.FileHandling ( todoExtension )
import Command.Dispatcher ( dispatch )
import Command.Dispatcher.Internal

spec :: Spec
spec = do
    describe "dispatch help" $ do

      it "Dispatching help command" $ do
       config <- configAndCleanUpDir
       (output, _) <- capture $ dispatch config ["help"]
       (outputUsage, _) <- capture usage
       output `shouldBe` outputUsage
    
    describe "dispatch ls" $ do
      
      it "Dispatching ls command with no to-do list to show" $ do
       config <- loadTestConfig
       (output, _) <- capture $ dispatch config ["ls"]
       (outptNoLists, _) <- capture notTodoListToShowMsg
       cleanUpDir
       output `shouldBe` outptNoLists
    
      it "Dispatching ls command with arguments" $ do
       config <- configAndCleanUpDir
       (output, _) <- capture $ dispatch config ["ls", "something"]
       (outputNoSuchComm, _) <- capture $ notSuchCommandError "ls"
       output `shouldBe` outputNoSuchComm
    
      it "Dispatching ls command with one to-do list" $ do
       config <- loadTestConfig
       let todoLst = joinPath [path config, "work.todo"]
       new todoLst
       (output, _) <- capture $ dispatch config ["ls"]
       output `shouldBe` "work\n"
       cleanUpDir
    
      it "Dispatching ls command with more than one to-do list" $ do
       config <- loadTestConfig
       let workLst = joinPath [path config, "work.todo"]
           homeLst = joinPath [path config, "home.todo"]
       new workLst
       new homeLst
       (output, _) <- capture $ dispatch config ["ls"]
       let sameContent = output `elem` ["work\nhome\n", "home\nwork\n"]
       sameContent `shouldBe` True
       cleanUpDir
    
    describe "dispatch new" $ do
      
      it "Dispatching new with a not existing to-do list" $ do
       config <- loadTestConfig
       dispatch config ["new", "work"]
       exists <- doesFileExist $ joinPath [path config, "work.todo"]
       exists `shouldBe` True
       cleanUpDir
    
      it "Dispatching new with a yet existing to-do list" $ do
       config <- loadTestConfig
       dispatch config ["new", "work"]
       (output, _) <- capture $ dispatch config ["new", "work"]
       (outputError, _) <- capture $ alreadyExistsListError "work"
       output `shouldBe` outputError
       cleanUpDir
    
      it "Dispatching new with too many arguments" $ do
       config <- loadTestConfig
       (output, _) <- capture $ dispatch config ["new", "work", "something"]
       (outputError, _) <- capture $ notSuchCommandError "new"
       output `shouldBe` outputError
       cleanUpDir
    
    describe "dispatch dl" $ do

      it "Dispatching dl with a not existing to-do list" $ do
       config <- loadTestConfig
       (output, _) <- capture $ dispatch config ["dl", "work"]
       (outputError, _) <- capture $ noSuchListError "work"
       cleanUpDir
       output `shouldBe` outputError
    
      it "Dispatching dl with too may arguments" $ do
       config <- loadTestConfig
       (output, _) <- capture $ dispatch config ["dl", "work", "something"]
       (outputError, _) <- capture $ notSuchCommandError "dl"
       cleanUpDir
       output `shouldBe` outputError
    
      it "Dispatching dl with an existing to-do list" $ do
       config <- loadTestConfig
       dispatch config ["new", "work"]
       dispatch config ["dl", "work"]
       newConfig <- loadTestConfig
       defaultList newConfig `shouldBe` Just "work"
       cleanUpDir
    
    describe "dispatch rename a remove on dl" $ do

      it "Dispatching rename default to-do list" $ do
       config <- loadTestConfig
       dispatch config ["new", "work"]
       dispatch config ["dl", "work"]
       newConfig <- loadTestConfig
       let newName = "job"
       dispatch newConfig ["rename", "work", newName]
       renamedDlConfig <- loadTestConfig
       defaultList renamedDlConfig `shouldBe` Just newName
       cleanUpDir
    
      it "Dispathcin remove on the default to-do list" $ do
       config <- loadTestConfig
       dispatch config ["new", "work"]
       dispatch config ["dl", "work"]
       newConfig <- loadTestConfig
       dispatch newConfig ["remove", "work"]
       removedDlConfig <- loadTestConfig
       defaultList removedDlConfig `shouldBe` Nothing 
       cleanUpDir
    
    describe "dispatch add" $ do

      it "Dispatching add task into a not existing list" $ do
       config <- loadTestConfig
       let notExistingLst = "work"
       (output, _) <- capture $ dispatch config ["add", notExistingLst, "Some task"]
       (error, _) <- capture $ noSuchListError notExistingLst
       output `shouldBe` error
       cleanUpDir
      
      it "Dispatching add task into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           task = "Some task"
       dispatch config ["new", todoLst]
       (output, _) <- capture $ dispatch config ["add", todoLst, task]
       insertedTask <- readFile $ joinPath [path config, todoLst ++ todoExtension]
       (output, insertedTask) `shouldBe` ("", task ++ "\n")
       cleanUpDir
    
      it "Dispatching add empty task into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           emptyTask = ""
       dispatch config ["new", todoLst]
       (output, _) <- capture $ dispatch config ["add", todoLst, emptyTask]
       (outputError, _) <- capture $ putErrorLn emptyTaskMsg
       output `shouldBe` outputError
       cleanUpDir
      
      it "Dispatching add only space task into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           spacedTask = "  "
       dispatch config ["new", todoLst]
       (output, _) <- capture $ dispatch config ["add", todoLst, spacedTask]
       (outputError, _) <- capture $ putErrorLn emptyTaskMsg
       output `shouldBe` outputError
       cleanUpDir
    
      it "Dispatching add task twice into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           firstTask = "First task"
           secondTask = "Second task"
       dispatch config ["new", todoLst]
       capture $ dispatch config ["add", todoLst, firstTask]
       capture $ dispatch config ["add", todoLst, secondTask]
       insertedTasks <- readFile $ joinPath [path config, todoLst ++ todoExtension]
       insertedTasks `shouldBe` firstTask ++ "\n" ++ secondTask ++ "\n"
       cleanUpDir
    
    describe "dispatch add -b" $ do

      it "Dispatching add -b task into a not existing list" $ do
       config <- loadTestConfig
       let notExistingLst = "work"
       (output, _) <- capture $ dispatch config ["add", "-b", notExistingLst, "Some task"]
       (error, _) <- capture $ noSuchListError notExistingLst
       output `shouldBe` error
       cleanUpDir
      
      it "Dispatching add -b task into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           task = "Some task"
       dispatch config ["new", todoLst]
       (output, _) <- capture $ dispatch config ["add", "-b",todoLst, task]
       insertedTask <- readFile $ joinPath [path config, todoLst ++ todoExtension]
       (output, insertedTask) `shouldBe` ("", task ++ "\n")
       cleanUpDir
    
      it "Dispatching add -b empty task into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           emptyTask = ""
       dispatch config ["new", todoLst]
       (output, _) <- capture $ dispatch config ["add", "-b",todoLst, emptyTask]
       (outputError, _) <- capture $ putErrorLn emptyTaskMsg
       output `shouldBe` outputError
       cleanUpDir
    
      it "Dispatching add -b only space task into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           spacedTask = "  "
       dispatch config ["new", todoLst]
       (output, _) <- capture $ dispatch config ["add", "-b", todoLst, spacedTask]
       (outputError, _) <- capture $ putErrorLn emptyTaskMsg
       output `shouldBe` outputError
       cleanUpDir
    
      it "Dispatching add -b task twice into an existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           firstTask = "First task"
           secondTask = "Second task"
       dispatch config ["new", todoLst]
       capture $ dispatch config ["add", "-b", todoLst, firstTask]
       capture $ dispatch config ["add", "-b", todoLst, secondTask]
       insertedTasks <- readFile $ joinPath [path config, todoLst ++ todoExtension]
       insertedTasks `shouldBe` secondTask ++ "\n" ++ firstTask ++ "\n"
       cleanUpDir

    describe "dispatch view" $ do
      
      it "Dispatching view with a not existing list" $ do
       config <- loadTestConfig
       let todoLst = "work"
       (ouput, _) <- capture $ dispatch config ["view", todoLst]
       (errorMsg, _) <- capture $ noSuchListError todoLst
       ouput `shouldBe` errorMsg
       cleanUpDir
      
      it "Dispatching view with an existing empty list" $ do
       config <- loadTestConfig
       let todoLst = "work"
       dispatch config ["new", todoLst]
       (ouput, _) <- capture $ dispatch config ["view", todoLst]
       (emptyLstMsg, _) <- capture . putStrLn . emptyListMsg $ joinPath [path config, todoLst ++ ".todo"]
       ouput `shouldBe` emptyLstMsg
       cleanUpDir

      it "Dispatching view with a singleton list" $ do
       config <- loadTestConfig
       let todoLst = "work"
           task = "Some task"
       dispatch config ["new", todoLst]
       dispatch config ["add", todoLst, task]
       (ouput, _) <- capture $ dispatch config ["view", todoLst]
       ouput `shouldBe` "0 - " ++ task ++ "\n\n"
       cleanUpDir
      
      it "Dispatching view with two task" $ do
       config <- loadTestConfig
       let todoLst = "work"
           firstTask = "First task"
           secondTask = "Second task"
       dispatch config ["new", todoLst]
       dispatch config ["add", todoLst, firstTask]
       dispatch config ["add", todoLst, secondTask]
       (ouput, _) <- capture $ dispatch config ["view", todoLst]
       ouput `shouldBe` "0 - " ++ firstTask ++ "\n" ++ "1 - " ++ secondTask ++ "\n\n"
       cleanUpDir
    
    describe "dispatch mv" $ do

      it "Dispatching mv from and to not existing list" $ do
       config <- loadTestConfig
       let notExistingLstFrom = "work"
           notExistingLstTo = "job"
       (output, _) <- capture $ dispatch config ["mv", notExistingLstFrom, "0", notExistingLstTo]
       (error, _) <- capture $ noSuchListError notExistingLstFrom
       output `shouldBe` error
       cleanUpDir
      
      it "Dispatching mv from an existing list to a not existing list" $ do
       config <- loadTestConfig
       let listFrom = "work"
           notExistingLstTo = "job"
           taskToMove = "Some task"
       dispatch config ["new", listFrom]
       dispatch config ["add", listFrom, taskToMove]
       (output, _) <- capture $ dispatch config ["mv", listFrom, "0", notExistingLstTo]
       (error, _) <- capture $ noSuchListError notExistingLstTo
       output `shouldBe` error
       cleanUpDir
      
      it "Dispatching mv with a dummy task index" $ do
       config <- loadTestConfig
       let listFrom = "work"
           listTo = "job"
           taskToMove = "Some task"
           dummyTaskIndex = "wawa"
       dispatch config ["new", listFrom]
       dispatch config ["new", listTo]
       dispatch config ["add", listFrom, taskToMove]
       (output, _) <- capture $ dispatch config ["mv", listFrom, dummyTaskIndex, listTo]
       (error, _) <- capture $ outOfBoundError dummyTaskIndex
       output `shouldBe` error
       cleanUpDir
      
      it "Dispatching mv with a out of bound index" $ do
       config <- loadTestConfig
       let listFrom = "work"
           listTo = "job"
           taskToMove = "Some task"
           dummyTaskIndex = "1"
       dispatch config ["new", listFrom]
       dispatch config ["new", listTo]
       dispatch config ["add", listFrom, taskToMove]
       (output, _) <- capture $ dispatch config ["mv", listFrom, dummyTaskIndex, listTo]
       (error, _) <- capture $ outOfBoundError dummyTaskIndex
       output `shouldBe` error
       cleanUpDir

      it "Dispatching mv happy path" $ do
       config <- loadTestConfig
       let listFrom = "work"
           listTo = "job"
           taskToMove = "Some task"
           taskIndex = "0"
       dispatch config ["new", listFrom]
       dispatch config ["new", listTo]
       dispatch config ["add", listFrom, taskToMove]
       (output, _) <- capture $ dispatch config ["mv", listFrom, taskIndex, listTo]
       output `shouldBe` ""
       cleanUpDir