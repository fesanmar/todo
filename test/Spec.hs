import Test.Hspec        (Spec, it, shouldBe, runIO)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Config
import System.FilePath
import System.Directory


baseDirName :: FilePath
baseDirName = joinPath [".", "test", "res"]

basePath :: FilePath
basePath = joinPath [baseDirName, ".todo"]

iniFilePath :: FilePath
iniFilePath = joinPath [basePath, "todo.ini"]

cleanUpDir :: IO ()
cleanUpDir = removePathForcibly basePath

main :: IO ()
main =  hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    
    -- Config
    it "Loading config creates .todo dir inside base path" $ do
     config <- loadConfig basePath 
     path config `shouldBe` basePath 
     cleanUpDir
    
    it "Loading config set ini.todo file" $ do
     config <- loadConfig basePath 
     configFilePath config `shouldBe` iniFilePath
     cleanUpDir
    
    it "Loading config default todo list as Nothing if todo.ini doesn't exists" $ do
     config <- loadConfig basePath
     cleanUpDir
     defaultList config `shouldBe` Nothing 
    
    it "Seting default to-do list, saving config and reloading it" $ do
     config <- loadConfig basePath 
     let modifiedConfig = newDefaultList (joinPath [basePath, "work"]) config
     dumpConfig modifiedConfig
     newLoadedConfig <- loadConfig basePath 
     newLoadedConfig `shouldBe` modifiedConfig
     cleanUpDir
