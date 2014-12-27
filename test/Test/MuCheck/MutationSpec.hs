module Test.MuCheck.MutationSpec (main, spec) where
import Test.Hspec
import Test.QuickCheck hiding ((==>))
import System.Random
import qualified Test.MuCheck.Mutation as Mu
import Test.MuCheck.MuOp
import Language.Haskell.Exts

main :: IO ()
main = hspec spec

m1 = Match (SrcLoc "<unknown>.hs" 15 1)
           (Ident "qs")
           [PApp (UnQual (Ident "M")) [],PLit Signless (Int 0)]
           Nothing
           (UnGuardedRhs (Lit (Int 1)))
           (BDecls [])
m2 = Match (SrcLoc "<unknown>.hs" 16 1)
           (Ident "qs")
           [PApp (UnQual (Ident "M")) [],PVar (Ident "i")]
           Nothing
           (UnGuardedRhs (Var (UnQual (Ident "i"))))
           (BDecls [])
m3 = Match (SrcLoc "<unknown>.hs" 17 1)
           (Ident "qs")
           [PApp (UnQual (Ident "N")) [],PWildCard]
           Nothing
           (UnGuardedRhs (NegApp (Lit (Int 1))))
           (BDecls [])

fb = FunBind [m1,m2,m3]

spec :: Spec
spec = do
  describe "removeOnePMatch" $ do
    it "for functions, generates subsets with one pattern removed" $ do
      (Mu.removeOnePMatch fb) `shouldBe` [fb ==> FunBind [m1,m2], fb ==> FunBind [m1,m3], fb ==> FunBind [m2,m3]]
    it "If empty, dont transform" $ do
      Mu.removeOnePMatch (FunBind [m1]) `shouldBe` []

  describe "permMatches" $ do
    it "for functions, generates sets with permutation of guards" $ do
      (Mu.permMatches fb) `shouldBe` [fb ==> FunBind [m2,m1,m3],
                                      fb ==> FunBind [m3,m2,m1],
                                      fb ==> FunBind [m2,m3,m1],
                                      fb ==> FunBind [m3,m1,m2],
                                      fb ==> FunBind [m1,m3,m2]]

