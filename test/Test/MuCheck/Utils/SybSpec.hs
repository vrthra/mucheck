module Test.MuCheck.Utils.SybSpec where

import Test.Hspec
import System.Random
import qualified Test.MuCheck.Utils.Syb as S
import Control.Monad (MonadPlus, mplus, mzero)
import Test.MuCheck.MuOp (mkMpMuOp, MuOp)
import Data.Generics (GenericQ, mkQ, Data, Typeable, mkMp, listify)
import Language.Haskell.Exts

main :: IO ()
main = hspec spec

m1 a b = Match (SrcLoc "<unknown>.hs" 15 1)
           (Ident a)
           [PApp (UnQual (Ident b)) [],PLit Signless (Int 0)]
           Nothing
           (UnGuardedRhs (Lit (Int 1)))
           (BDecls [])

replM :: MonadPlus m => Name -> m Name
replM (Ident "x") = return $ Ident "y"
replM t = mzero


spec :: Spec
spec = do
  describe "once" $ do
    it "apply a function once on exp" $ do
      (S.once (mkMp replM) (FunBind [m1 "y" "x"]) :: Maybe Decl) `shouldBe` Just (FunBind [m1 "y" "y"] :: Decl)
    it "apply a function just once" $ do
      (S.once (mkMp replM) (FunBind [m1 "x" "x"]) :: Maybe Decl) `shouldBe` Just (FunBind [m1 "y" "x"] :: Decl)
    it "apply a function just once if possible" $ do
      (S.once (mkMp replM) (FunBind [m1 "y" "y"]) :: Maybe Decl) `shouldBe` Nothing 
    it "should return all possibilities" $ do
      (S.once (mkMp replM) (FunBind [m1 "x" "x"]) :: [Decl]) `shouldBe`  ([FunBind [m1 "y" "x"], FunBind [m1 "x" "y"]] :: [Decl])

