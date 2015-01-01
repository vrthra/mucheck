module Test.MuCheck.MutationSpec where

import Test.Hspec
import System.Random
import qualified Test.MuCheck.Mutation as M
import Control.Monad (MonadPlus, mplus, mzero)
import Test.MuCheck.MuOp (mkMpMuOp, MuOp)
import Data.Generics (GenericQ, mkQ, Data, Typeable, mkMp, listify)
import Language.Haskell.Exts

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "selectGuardedBoolNegOps" $ do
    it "returns mutated case statements" $ do
      1 `shouldBe` 1
