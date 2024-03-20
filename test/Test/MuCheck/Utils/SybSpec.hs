module Test.MuCheck.Utils.SybSpec where

import Test.Hspec
import qualified Test.MuCheck.Utils.Syb as S
import Control.Monad (MonadPlus, mplus, mzero)
import Test.MuCheck.MuOp (mkMpMuOp, MuOp)
import Data.Generics (GenericQ, mkQ, Data, Typeable, mkMp, listify)
import Language.Haskell.Exts

tempSrcLoc = SrcLoc "<unknown>.hs" 15 1

main :: IO ()
main = hspec spec


m1 a b = Match (SrcLoc "<unknown>.hs" 15 1)
           (Ident tempSrcLoc a)
           [PApp tempSrcLoc (UnQual tempSrcLoc (Ident tempSrcLoc b)) [],PLit tempSrcLoc (Signless tempSrcLoc) (Int tempSrcLoc 0 "0")]
           (UnGuardedRhs tempSrcLoc (Lit tempSrcLoc (Int tempSrcLoc 1 "1")))
           (Just (BDecls tempSrcLoc []))

replM :: MonadPlus m => Name SrcLoc -> m (Name SrcLoc)
replM (Ident l "x") = return $ Ident l "y"
replM t = mzero

spec :: Spec
spec = do
  describe "once" $ do
    it "apply a function once on exp" $ do
      (S.once (mkMp replM) (FunBind tempSrcLoc [m1 "y" "x"]) :: Maybe (Decl SrcLoc)) `shouldBe` Just (FunBind tempSrcLoc [m1 "y" "y"] :: (Decl SrcLoc))
    it "apply a function just once" $ do
      (S.once (mkMp replM) (FunBind tempSrcLoc [m1 "x" "x"]) :: Maybe (Decl SrcLoc)) `shouldBe` Just (FunBind tempSrcLoc [m1 "y" "x"] :: (Decl SrcLoc))
    it "apply a function just once if possible" $ do
      (S.once (mkMp replM) (FunBind tempSrcLoc [m1 "y" "y"]) :: Maybe (Decl SrcLoc)) `shouldBe` Nothing 
    it "should return all possibilities" $ do
      (S.once (mkMp replM) (FunBind tempSrcLoc [m1 "x" "x"]) :: [(Decl SrcLoc)]) `shouldBe`  ([FunBind tempSrcLoc [m1 "y" "x"], FunBind tempSrcLoc [m1 "x" "y"]] :: [(Decl SrcLoc)])
