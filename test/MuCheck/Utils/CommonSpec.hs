module MuCheck.Utils.CommonSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import MuCheck.Utils.Common

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "safeHead" $ do
    it "gets Nothing if empty list" $ do
      safeHead ([]::[Int]) `shouldBe` (Nothing :: Maybe Int)

    it "gets Just head if list contains elements" $ do
      safeHead [1] `shouldBe` (Just 1 :: Maybe Int)

    it "gets Just head if list contains elements - property" $
      property $ \x xs -> safeHead (x:xs) == (Just x :: Maybe Int)
