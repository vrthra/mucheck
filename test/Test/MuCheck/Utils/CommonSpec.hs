module Test.MuCheck.Utils.CommonSpec (main, spec) where
import Test.Hspec
import System.Random
import Test.MuCheck.Utils.Common (replaceFst, choose, remElt, sample, sampleF, coupling)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "replaceFst" $ do
    it "if given empty list dont do any thing" $ do
      replaceFst (1,2) ([]::[Int]) `shouldBe` ([] :: [Int])

    it "if given a list with out value dont do any thing" $ do
      replaceFst (1,2) ([3]::[Int]) `shouldBe` ([3] :: [Int])

    it "if given a list with value replaceFst" $ do
      replaceFst (1,2) ([1]::[Int]) `shouldBe` ([2] :: [Int])

  describe "choose" $ do
    it "if given empty return empty" $ do
      choose ([]::[Int]) 10 `shouldBe` ([]::[[Int]])
    it "if given zero return e.empty" $ do
      choose [1] 0 `shouldBe` [[]]
    it "if given list return subset with given size" $ do
      choose [1,2,3] 2 `shouldBe` [[1,2], [1,3], [2,3]]

  describe "remElt" $ do
    it "must remove element at given index" $ do
      remElt 2 [1,2,3,4] `shouldBe` [1,2,4]

  describe "sample" $ do
    it "must sample a given size subset" $ do
      sample (mkStdGen 1) 2 [1,2,3,4] `shouldBe` [2, 3]

  describe "sampleF" $ do
    it "must sample a given fraction subset" $ do
      sampleF (mkStdGen 1) 0.5 [1,2,3,4] `shouldBe` [2, 3]

  describe "coupling" $ do
    it "must sample a given fraction subset" $ do
      coupling (+) [1,2,3] `shouldBe` [3,4,3,5,4,5]
