module Test.MuCheck.MutationSpec where

import Test.Hspec
import System.Random
import Test.MuCheck.Mutation (getASTFromStr, mutate, mutatesN,
  selectGuardedBoolNegOps,
  selectIfElseBoolNegOps,
  selectLitOps,
  selectBLitOps,
  selectFnMatches)
import Control.Monad (MonadPlus, mplus, mzero)
import Test.MuCheck.MuOp (mkMpMuOp, MuOp, (==>))
import Data.Generics (GenericQ, mkQ, Data, Typeable, mkMp, listify)
import Language.Haskell.Exts

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "selectGuardedBoolNegOps" $ do
    it "returns relevant function guard ordering mutators" $ do
      let text = "myFn x | x == 1 = True\nmyFn   | otherwise = False\n"
      selectGuardedBoolNegOps (getASTFromStr text) `shouldBe` [
       GuardedRhs (SrcLoc "<unknown>.hs" 1 8)
                  [Qualifier (InfixApp (Var (UnQual (Ident "x"))) (QVarOp (UnQual (Symbol "=="))) (Lit (Int 1)))]
                  (Con (UnQual (Ident "True")))
        ==>
        GuardedRhs (SrcLoc "<unknown>.hs" 1 8)
                  [Qualifier (App (Var (UnQual (Ident "not"))) (InfixApp (Var (UnQual (Ident "x"))) (QVarOp (UnQual (Symbol "=="))) (Lit (Int 1))))]
                  (Con (UnQual (Ident "True"))),
        GuardedRhs (SrcLoc "<unknown>.hs" 2 8)
                  [Qualifier (Var (UnQual (Ident "otherwise")))]
                  (Con (UnQual (Ident "False")))
        ==>
        GuardedRhs (SrcLoc "<unknown>.hs" 2 8)
                   [Qualifier (Var (UnQual (Ident "otherwise")))]
                   (Con (UnQual (Ident "False")))
        ]


  describe "selectIfElseBoolNegOps" $ do
    it "returns relevant ifElse mutators" $ do
      let text = "myFn x = if True then 1 else 0\n"
      selectIfElseBoolNegOps (getASTFromStr text) `shouldBe` [
        If (Con (UnQual (Ident "True"))) (Lit (Int 1)) (Lit (Int 0))
        ==>
        If (Con (UnQual (Ident "True"))) (Lit (Int 0)) (Lit (Int 1))]

  describe "selectLitOps" $ do
    it "returns relevant literal mutators" $ do
      let text = "myFn x = if True then 1 else 0\n"
      selectLitOps (getASTFromStr text) `shouldBe` [
        Int 1 ==> Int 2,
        Int 1 ==> Int 0,
        Int 1 ==> Int 1,
        Int 0 ==> Int 1,
        Int 0 ==> Int (-1),
        Int 0 ==> Int 0]

  describe "selectBLitOps" $ do
    it "returns relevant literal mutators" $ do
      let text = "myFn x = if True then 1 else 0\n"
      selectBLitOps (getASTFromStr text) `shouldBe` [
        Ident "True" ==> Ident "False"]

  describe "selectFnMatches" $ do
    it "returns mutated function definition by removing one of the patterns" $ do
      let text = "myFn True = 1\nmyFn False = 0\n"
      selectFnMatches (getASTFromStr text) `shouldBe` [
        FunBind [Match (SrcLoc "<unknown>.hs" 1 1)
                       (Ident "myFn")
                       [PApp (UnQual (Ident "True")) []]
                       Nothing
                       (UnGuardedRhs (Lit (Int 1)))
                       (BDecls []),
                Match (SrcLoc "<unknown>.hs" 2 1)
                      (Ident "myFn")
                      [PApp (UnQual (Ident "False")) []]
                      Nothing
                      (UnGuardedRhs (Lit (Int 0)))
                      (BDecls [])]
        ==>
        FunBind [Match (SrcLoc "<unknown>.hs" 1 1)
                      (Ident "myFn")
                      [PApp (UnQual (Ident "True")) []]
                      Nothing
                      (UnGuardedRhs (Lit (Int 1)))
                      (BDecls []),
                Match (SrcLoc "<unknown>.hs" 2 1)
                      (Ident "myFn")
                      [PApp (UnQual (Ident "False")) []]
                      Nothing
                      (UnGuardedRhs (Lit (Int 0)))
                      (BDecls [])] ]
  describe "mutate" $ do
    it "returns relevant mutations" $ do
      let text = "myFn x = if True then 1 else 0\n"
      mutate (Int 1 ==> Int 2) (getASTFromStr text) `shouldBe`[
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind
            [Match (SrcLoc "<unknown>.hs" 1 1)
                   (Ident "myFn")
                   [PVar (Ident "x")]
                   Nothing
                   (UnGuardedRhs (If (Con (UnQual (Ident "True"))) (Lit (Int 2)) (Lit (Int 0))))
                   (BDecls [])]] ]

  describe "mutatesN" $ do
    it "returns relevant mutations" $ do
      let text = "myFn x = if True then 1 else 0\n"
      mutatesN [Int 1 ==> Int 2] (getASTFromStr text) 1 `shouldBe`[
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind
            [Match (SrcLoc "<unknown>.hs" 1 1)
                   (Ident "myFn")
                   [PVar (Ident "x")]
                   Nothing
                   (UnGuardedRhs (If (Con (UnQual (Ident "True"))) (Lit (Int 2)) (Lit (Int 0))))
                   (BDecls [])]] ]
    it "returns all relevant mutations" $ do
      let text = "myFn x = if 1 then 1 else 0\n"
      mutatesN [Int 1 ==> Int 2] (getASTFromStr text) 1 `shouldBe`[
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind [Match (SrcLoc "<unknown>.hs" 1 1) (Ident "myFn") [PVar (Ident "x")] Nothing (UnGuardedRhs (If (Lit (Int 2)) (Lit (Int 1)) (Lit (Int 0)))) (BDecls [])]],
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind [Match (SrcLoc "<unknown>.hs" 1 1) (Ident "myFn") [PVar (Ident "x")] Nothing (UnGuardedRhs (If (Lit (Int 1)) (Lit (Int 2)) (Lit (Int 0)))) (BDecls [])]] ]

