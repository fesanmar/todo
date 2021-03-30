import Test.Hspec        (Spec, it, shouldBe, runIO)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Config

import Todo.List
import Todo.List.Internal
import System.FilePath
import System.Directory
import System.IO.Silently
import Command.Dispatcher
import Command.Dispatcher.Internal



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
    
    -- To-do list level operations

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
     (ouput, _) <- capture $ new todoLst
     (outputError, _) <- capture $ alreadyExistsListError "work"
     cleanUpDir
     ouput `shouldBe` outputError
    
    -- Dispatch
    
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