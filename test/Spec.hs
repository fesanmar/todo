import Test.Hspec        (Spec, it, shouldBe, runIO)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import qualified Data.ByteString.UTF8 as BUT
import Data.Either
import App.Config
import App.Config.Internal
import Todo.FileHandling
import Todo.List
import Todo.Task
import Todo.List.Internal
import System.FilePath
import System.Directory
import System.IO.Silently
import Command.Dispatcher
import Command.Dispatcher.Internal
import App.Messages



baseDirName :: FilePath
baseDirName = joinPath [".", "test", "res"]

basePath :: FilePath
basePath = joinPath [baseDirName, ".todo"]

iniFilePath :: FilePath
iniFilePath = joinPath [basePath, "todo.ini"]

cleanUpDir :: IO ()
cleanUpDir = removePathForcibly basePath

loadTestConfig :: IO Config
loadTestConfig = loadConfig basePath

configAndCleanUpDir :: IO Config
configAndCleanUpDir = do 
    config <- loadConfig basePath 
    cleanUpDir
    return config

main :: IO ()
main =  hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    
    -- Config
    it "Fetching key value pairs form ini file content" $
     let content = "; comment\n[Section]\nmyKey=value\n;commentLike=keyValue\nnothing, ignore me.\n"
         keyValuePairs = extractKVPairs $ BUT.fromString content in
     keyValuePairs `shouldBe` [("myKey","value")]

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
    
    it "Seting default to-do list, saving config and reloading it" $ do
     config <- loadTestConfig
     let modifiedConfig = newDefaultList "work" config
     dumpConfig modifiedConfig
     newLoadedConfig <- loadTestConfig
     newLoadedConfig `shouldBe` modifiedConfig
     cleanUpDir

    it "Casting empty config to list" $ do
     config <- configAndCleanUpDir
     configToList config `shouldBe` []

    it "Casting a not empty config to list" $ do
     config <- configAndCleanUpDir
     let modifiedConfig = newDefaultList "work" config
     configToList modifiedConfig `shouldBe` ["defaultList=work"]
    
    -- List

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

    -- Task

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
    
    -- Dispatcher
    
    it "Dispatching help command" $ do
     config <- configAndCleanUpDir
     (output, _) <- capture $ dispatch config ["help"]
     (outputUsage, _) <- capture usage
     output `shouldBe` outputUsage
    
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
     output `shouldBe` "work\nhome\n"
     cleanUpDir
    
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
     (outputError, _) <- capture emptyTaskError
     output `shouldBe` outputError
     cleanUpDir
    
    it "Dispatching add empty task into an existing list" $ do
     config <- loadTestConfig
     let todoLst = "work"
         spacedTask = "  "
     dispatch config ["new", todoLst]
     (output, _) <- capture $ dispatch config ["add", todoLst, spacedTask]
     (outputError, _) <- capture emptyTaskError
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