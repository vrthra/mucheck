module MuCheck.Utils.PrintSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified MuCheck.Utils.Print as MUP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showPercent" $ do
    it "shows percent values correctly" $ do
      MUP.showPerCent 10 `shouldBe` "(10%)"

  describe "showAS" $ do
    it "joins strings with newlines" $ do
      MUP.showAS ["a","b"] `shouldBe` "a\nb"

  describe "showA" $ do
    it "joins values with newlines" $ do
      MUP.showA [1,2] `shouldBe` "1\n2"

