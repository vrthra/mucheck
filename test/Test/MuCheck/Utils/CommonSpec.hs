module Test.MuCheck.Utils.CommonSpec (main, spec) where
import Test.Hspec
import System.Random
import qualified Test.MuCheck.Utils.Common as MUC

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "replace" $ do
    it "if given empty list dont do any thing" $ do
      MUC.replace (1,2) ([]::[Int]) `shouldBe` ([] :: [Int])

    it "if given a list with out value dont do any thing" $ do
      MUC.replace (1,2) ([3]::[Int]) `shouldBe` ([3] :: [Int])

    it "if given a list with value replace" $ do
      MUC.replace (1,2) ([1]::[Int]) `shouldBe` ([2] :: [Int])

  describe "genFileNames" $ do
    it "if given list, generate filenames" $ do
      head (MUC.genFileNames "mytest.hs") `shouldBe` "mytest_1.hs"

  describe "choose" $ do
    it "if given empty return empty" $ do
      MUC.choose ([]::[Int]) 10 `shouldBe` ([]::[[Int]])
    it "if given zero return e.empty" $ do
      MUC.choose [1] 0 `shouldBe` [[]]
    it "if given list return subset with given size" $ do
      MUC.choose [1,2,3] 2 `shouldBe` [[1,2], [1,3], [2,3]]

  describe "remElt" $ do
    it "must remove element at given index" $ do
      MUC.remElt 2 [1,2,3,4] `shouldBe` [1,2,4]

  describe "sample" $ do
    it "must sample a given size subset" $ do
      (MUC.sample (mkStdGen 1) 2 [1,2,3,4]) `shouldBe` [2, 3]

  describe "sampleF" $ do
    it "must sample a given fraction subset" $ do
      (MUC.sampleF (mkStdGen 1) 0.5 [1,2,3,4]) `shouldBe` [2, 3]

  describe "coupling" $ do
    it "must sample a given fraction subset" $ do
      (MUC.coupling (+) [1,2,3]) `shouldBe` [3,4,3,5,4,5]
