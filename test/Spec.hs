import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Util.ScrollList (up, down)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do
    -- Moving up
    it "Moving up an out of range index" $
     up 4 1 toThree `shouldBe` toThree

    it "Moving up a negative index" $
     up (-1) 1 toThree `shouldBe` toThree

    it "Move task one position up" $
     up 1 1 toThree `shouldBe` ["two", "one", "three"]

    it "Move task two positions up" $
     up 2 2 toThree `shouldBe` ["three", "one", "two"]

    it "Move task too many positions up" $
     up 2 3 toThree `shouldBe` ["three", "one", "two"]

    it "Move last task one position up" $
     up 2 1 toThree `shouldBe` ["one", "three","two"]

    it "Move no position up" $
     up 2 0 toThree `shouldBe` toThree

    it "Move negative position up" $
     up 2 (-2) toThree `shouldBe` toThree

    it "Move task three positions up in a longer list" $
     up 7 3 toTen `shouldBe` ["one", "two", "three", "four", "eight", "five", "six", "seven", "nine", "ten"]

    -- Moving down
    it "Moving dow an out of range index" $
     down 4 1 toThree `shouldBe` toThree

    it "Moving down a negative index" $
     down (-1) 1 toThree `shouldBe` toThree

    it "Moving down a negative index as bigger as list's lengt" $
     down (-3) 1 toThree `shouldBe` toThree

    it "Moving down a negative index bigger than list's lengt" $
     down (-4) 1 toThree `shouldBe` toThree

    it "Move task one position down" $
     down 0 1 toThree `shouldBe` ["two", "one", "three"]

    it "Move task two positions down" $
     down 0 2 toThree `shouldBe` ["two", "three", "one"]

    it "Move task too many positions down" $
     down 0 4 toThree `shouldBe` ["two", "three", "one"]

    it "Move no position down" $
     down 2 0 toThree `shouldBe` toThree

    it "Move negative position down" $
     down 2 (-2) toThree `shouldBe` toThree

    it "Move negative position down" $
     down 2 (-2) toThree `shouldBe` toThree

    it "Move task three positions down in a longer list" $
     down 5 3 toTen `shouldBe` ["one", "two", "three", "four", "five", "seven", "eight", "nine", "six", "ten"]

    

toThree :: [String]
toThree = ["one", "two", "three"]

toTen :: [String]
toTen = toThree ++ ["four", "five", "six", "seven", "eight", "nine", "ten"]
