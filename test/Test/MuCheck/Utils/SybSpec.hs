module Test.MuCheck.Utils.SybSpec where

import Test.Hspec
import qualified Test.MuCheck.Utils.Syb as S
import Control.Monad (MonadPlus, mplus, mzero)
import Test.MuCheck.MuOp (mkMpMuOp, MuOp)
import Data.Generics (GenericQ, mkQ, Data, Typeable, mkMp, listify)
import Language.Haskell.Exts

main :: IO ()
main = hspec spec

dummySrcLoc = SrcLoc "<unknown>.hs" 15 1

m1 a b = Match (dummySrcLoc)
           (Ident dummySrcLoc a)
           [PApp dummySrcLoc (UnQual dummySrcLoc (Ident dummySrcLoc b)) [],PLit dummySrcLoc (Signless dummySrcLoc) (Int dummySrcLoc 0 "0")]
           (UnGuardedRhs dummySrcLoc (Lit dummySrcLoc (Int dummySrcLoc 1 "1")))
           (Just (BDecls dummySrcLoc []))

replM :: MonadPlus m => Name SrcLoc -> m (Name SrcLoc)
replM (Ident l "x") = return $ Ident l "y"
replM t = mzero

spec :: Spec
spec = do
  describe "once" $ do
    it "apply a function once on exp" $ do
      (S.once (mkMp replM) (FunBind dummySrcLoc [m1 "y" "x"]) :: Maybe (Decl SrcLoc)) `shouldBe` Just (FunBind dummySrcLoc [m1 "y" "y"] :: (Decl SrcLoc))
    it "apply a function just once" $ do
      (S.once (mkMp replM) (FunBind dummySrcLoc [m1 "x" "x"]) :: Maybe (Decl SrcLoc)) `shouldBe` Just (FunBind dummySrcLoc [m1 "y" "x"] :: (Decl SrcLoc))
    it "apply a function just once if possible" $ do
      (S.once (mkMp replM) (FunBind dummySrcLoc [m1 "y" "y"]) :: Maybe (Decl SrcLoc)) `shouldBe` Nothing 
    it "should return all possibilities" $ do
      (S.once (mkMp replM) (FunBind dummySrcLoc [m1 "x" "x"]) :: [(Decl SrcLoc)]) `shouldBe`  ([FunBind dummySrcLoc [m1 "y" "x"], FunBind dummySrcLoc [m1 "x" "y"]] :: [(Decl SrcLoc)])
