import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Todo.Task ( up )

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    it "Moving up an out of range index" $
     up 4 1 toThree `shouldBe` toThree
    it "Move task one position up" $
     up 1 1 toThree `shouldBe` ["two", "one", "three"]
    it "Move task two positions up" $
     up 2 2 toThree `shouldBe` ["three", "one", "two"]
    it "Move task too many positions up" $
     up 2 3 toThree `shouldBe` ["three", "one", "two"]
    it "Move las task one position up" $
     up 2 1 toThree `shouldBe` ["one", "three","two"]
    it "Move no position up" $
     up 2 0 toThree `shouldBe` toThree
    it "Move negative position up" $
     up 2 (-2) toThree `shouldBe` toThree
    it "Move task three positions up in a longer list" $
     up 7 3 toTen `shouldBe` ["one", "two", "three", "four", "eight", "five", "six", "seven", "nine", "ten"]

toThree :: [String]
toThree = ["one", "two", "three"]

toTen :: [String]
toTen = toThree ++ ["four", "five", "six", "seven", "eight", "nine", "ten"]
