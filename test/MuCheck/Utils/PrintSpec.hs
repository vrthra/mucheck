module MuCheck.Utils.PrintSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import MuCheck.Utils.Print

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "./." $ do
    it "shows percent values correctly" $ do
      (./.) 5 10 `shouldBe` "(50%)"

  describe "showAS" $ do
    it "joins strings with newlines" $ do
      showAS ["a","b"] `shouldBe` "a\nb"

  describe "showA" $ do
    it "joins values with newlines" $ do
      showA [1,2] `shouldBe` "1\n2"

