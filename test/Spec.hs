{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
import Test.Hspec

import qualified MuCheck.Util.Common


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Common" MuCheck.Util.Common.spec
